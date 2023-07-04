import gurobipy as gp
import numpy as np
from gurobipy import GRB
import re
import pandas as pd
import os
from datetime import datetime
import time


def model_organize_results(var_values, var_df):
    counter = 0
    for v in var_values:
        # if(v.X>0):
        current_var = re.split("\[|,|]", v.varName)[:-1]
        current_var.append(round(v.X, 2))
        var_df.loc[counter] = current_var
        counter = counter + 1
        # with open("./math_model_outputs/" + 'mip-results.txt',
        #           "w") as f:  # a: open for writing, appending to the end of the file if it exists
        #     f.write(','.join(map(str, current_var)) + '\n')
        # print(','.join(map(str,current_var )))
    return var_df


def mathematical_model_solve(mip_inputs):
    model = gp.Model("uav_search")

    # add link variables - if the vehicle k moves from region i to j; 0, otherwise.
    x_tijk = model.addVars(
        mip_inputs.links,
        vtype=GRB.BINARY,
        name="x_tijk",
    )

    y_jmk = model.addVars(
        mip_inputs.targets_visits,
        vtype=GRB.BINARY,
        name="y_jmk",
    )

    t_h = model.addVar(
        lb=0,
        vtype=GRB.CONTINUOUS,
        name="t_h",
    )

    theta_k = model.addVars(
        mip_inputs.vehicle_list,
        vtype=GRB.CONTINUOUS,
        name="theta_k",
    )

    # set objective
    obj_max = gp.quicksum(mip_inputs.targets_expected_information[j]*y_jmk[j] for j in mip_inputs.targets_visits)
    penalty_coef_return_time = 10 ** -6
    obj_penalize_operation_time = penalty_coef_return_time * t_h
    model.setObjective(obj_max - obj_penalize_operation_time)

    # equations for radar detection threat restriction
    # constraint 2
    for k in mip_inputs.vehicle_list:
        model.addConstr(theta_k[k] <= (-np.log(1-mip_inputs.detection_limit)))

    # constraint 3
    for k in mip_inputs.vehicle_list:
        detection_coef_trajectories = {e: v for e, v in mip_inputs.links_detections.items() if e[3] == k}
        detection_coef_targets = {e: v for e, v in mip_inputs.targets_detections.items() if e[2] == k}
        model.addConstr(theta_k[k] == x_tijk.prod(detection_coef_trajectories, '*', '*', '*', k) + y_jmk.prod(detection_coef_targets, '*', '*', k))

    # constraint 4 - time limit for the mission
    model.addConstr(t_h <= mip_inputs.time_limit)

    # constraint 5 - find the amount of time to all UAVs return to the base (makespan)
    for k in mip_inputs.vehicle_list:
        duration_coef_trajectories = {d: v for d, v in mip_inputs.links_durations.items() if d[3] == k}
        duration_coef_targets = {d: v for d, v in mip_inputs.targets_search_duration.items() if d[2] == k}
        model.addConstr(t_h >= x_tijk.prod(duration_coef_trajectories, '*', '*', '*', k) + y_jmk.prod(duration_coef_targets, '*', '*', k))

    # constraint 6 - Each revisit of a target location can be performed by only one UAV.
    for j in mip_inputs.target_list:
        for m in mip_inputs.revisit_list:
            model.addConstr(y_jmk.sum(j, m, '*') <= 1)

    # constraint 7 - each revisit requires a former visit to the target.
    for j in mip_inputs.target_list:
        for m in mip_inputs.revisit_list:
            model.addConstr(y_jmk.sum(j, m, '*') >= y_jmk.sum(j, m+1, '*'))

    # constraint 8 - for each revisit, there has to be an incoming arc to the target
    for j in mip_inputs.target_list:
        for k in mip_inputs.vehicle_list:
            model.addConstr(y_jmk.sum(j, '*', k) <= x_tijk.sum('*', '*', j, k))

    # constraint 9 - each leaving vehicle has to return to the base at the end
    for k in mip_inputs.vehicle_list:
        model.addConstr(x_tijk.sum(1, 0, '*', k) == x_tijk.sum('*', '*', 0, k))

    # constraint 10 - a vehicle leaves only once from the base
    for k in mip_inputs.vehicle_list:
        model.addConstr(x_tijk.sum(1, 0, '*', k) == 1 )

    # constraint 11 - each leaving vehicle has to return to the base at the end
    for t in mip_inputs.time_step_list:
        for j in mip_inputs.target_list:
            for k in mip_inputs.vehicle_list:
                model.addConstr(x_tijk.sum(t, '*', j, k) == x_tijk.sum(t+1, j, '*', k))

    model.ModelSense = -1  # set objective to maximization
    model.params.TimeLimit = 3600
    model.params.MIPGap = 0.01
    start_time = time.time()
    # model.params.MIPFocus = 1
    model.params.Presolve = 2
    model.params.LogFile = "gurobi_log"

    # model.params.MIPGap = 0.02
    # model.params.Cuts = 3
    # model.params.Threads = 8
    # model.params.NoRelHeurTime = 5

    # model.update()
    # model.write("model_hand2.lp")
    # (23.745 - 23.39) == (24.1-23.745)
    # 23.745 - 23.390
    # 0.455*0.355
    model.update()
    model.printStats()
    model.optimize()
    end_time = time.time()
    run_time_cpu = round(end_time - start_time, 2)

    # for c in model.getConstrs():
    #     if c.Slack < 1e-6:
    #         print('Constraint %s is active at solution point' % (c.ConstrName))
    #

    if model.Status == GRB.Status.INFEASIBLE:
        # model.computeIIS()
        # model.write("infeasible_model.ilp")
        # print("Go check infeasible_model.ilp file")
        print("infeasible")

    else:

        x_tijk_results_df = pd.DataFrame(columns=['var_name', 'time_step', 'from_node_id', 'to_node_id', 'vehicle_id', 'value'])
        x_tijk_results_df = model_organize_results([v for v in x_tijk.values() if v.X > 0], x_tijk_results_df)
        # x_tijk_results_df = model_organize_results(x_tijk.values(), x_tijk_results_df)


        y_jmk_results_df = pd.DataFrame(columns=['var_name', 'target_id', 'revisit_number', 'vehicle_id', 'value'])
        y_jmk_results_df = model_organize_results([y for y in y_jmk.values() if y.X > 0], y_jmk_results_df)
        # y_jmk_results_df = model_organize_results(y_jmk.values(), y_jmk_results_df)

        t_h_result = t_h.X

        theta_k_results_df = pd.DataFrame(columns=['var_name', 'vehicle_id', 'value'])
        theta_k_results_df = model_organize_results(theta_k.values(), theta_k_results_df)

        # model global results
        obj_result = model.objval - penalty_coef_return_time * t_h_result

        global_results_df = pd.DataFrame(columns=['total_value', 'model_obj_value', 'model_obj_bound', 'gap', 'gurobi_time', 'python_time'])
        global_results_df.loc[len(global_results_df.index)] = [obj_result, model.objval, model.objbound, model.mipgap,
                                                               model.runtime, run_time_cpu]

        global_results_df["mission_time_limit"] = mip_inputs.time_limit
        global_results_df["mission_time"] = t_h_result

        global_results_df["detection_probability_limit"] = mip_inputs.detection_limit
        global_results_df["detection_count_limit"] = -np.log(1-mip_inputs.detection_limit)
        global_results_df["largest_detection"] = max(theta_k_results_df["value"])

        global_results_df["number_of_searches"] = sum(y_jmk_results_df.value > 0) # subtract the base return time
        global_results_df["number_of_vehicles"] = len(mip_inputs.vehicle_list)  # subtract the base return time


        if mip_inputs.experiment_mode == "single_objective":
            writer_file_name = os.path.join('outputs', "single_objective_results_{0}_nodes_{1}.xlsx".format(mip_inputs.n_targets,
                                                                                           str(datetime.now().strftime(
                                                                                               '%Y_%m_%d_%H_%M'))))
            writer = pd.ExcelWriter(writer_file_name)
            global_results_df.to_excel(writer, sheet_name='global_results')
            x_tijk_results_df.to_excel(writer, sheet_name='x_tijk_results')
            y_jmk_results_df.to_excel(writer, sheet_name='y_jmk_results')
            theta_k_results_df.to_excel(writer, sheet_name='theta_k_results')

            mip_inputs.target_df.to_excel(writer, sheet_name='inputs_target_df')
            mip_inputs.trajectory_df.to_excel(writer, sheet_name='inputs_trajectory_df')
            mip_inputs.parameters_df.to_excel(writer, sheet_name='inputs_parameters')
            writer.close()

        elif mip_inputs.experiment_mode == "multi_objective":
            writer_file_name_objective = os.path.join('outputs', "multi_objective_results_{0}.csv".format(mip_inputs.run_start_date))
            writer_file_name_solution = os.path.join('outputs', "multi_objective_results_{0}_{1}_{2}_{3}.xlsx".format(mip_inputs.time_limit, mip_inputs.detection_limit, mip_inputs.n_vehicles, str(datetime.now().strftime(
                                                                                               '%Y_%m_%d_%H_%M'))))
            if os.path.isfile(writer_file_name_objective):
                global_results_df.to_csv(writer_file_name_objective, mode="a", index=False, header=False)
            else:
                global_results_df.to_csv(writer_file_name_objective, mode="a", index=False, header=True)

            writer = pd.ExcelWriter(writer_file_name_solution)
            global_results_df.to_excel(writer, sheet_name='global_results')
            x_tijk_results_df.to_excel(writer, sheet_name='x_tijk_results')
            y_jmk_results_df.to_excel(writer, sheet_name='y_jmk_results')
            theta_k_results_df.to_excel(writer, sheet_name='theta_k_results')

            mip_inputs.target_df.to_excel(writer, sheet_name='inputs_target_df')
            mip_inputs.trajectory_df.to_excel(writer, sheet_name='inputs_trajectory_df')
            mip_inputs.parameters_df.to_excel(writer, sheet_name='inputs_parameters')
            writer.close()


        return global_results_df



