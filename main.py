# firefighting model by erdi dasdemir

# import required packages
import numpy as np
import pandas as pd

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
import mip_setup as mip_setup
import mip_solve as mip_solve
import openpyxl
from datetime import datetime
import os
from random import sample


# read user inputs
user_inputs = mip_setup.UserInputsRead()
experiment_mode = user_inputs.parameters_df.loc["mode", "value"]


# run optimization in single_run_mode
if experiment_mode == "single_objective":
    mip_inputs = mip_setup.InputsSetup(user_inputs)
    mip_inputs.experiment_mode = experiment_mode
    mip_solve.mathematical_model_solve(mip_inputs)

# run optimization in combination_mode
elif experiment_mode == "multi_objective":
    user_inputs.run_start_date = str(datetime.now().strftime('%Y_%m_%d_%H_%M'))
    vehicle_set = list(range(user_inputs.parameters_df.loc["n_vehicles", "value"], 1, -1))
    time_limit_set = list(range(user_inputs.parameters_df.loc["time_limit", "value"], 1, -4))
    detection_limit_set = sorted([round(elem, 2) for elem in np.arange(0.1, user_inputs.parameters_df.loc["probability_of_detection_limit", "value"], 0.1).tolist()], reverse=True)

    for time_limit_now in [24, 20, 16, 12, 8, 4]:  # time_limit_set:
        print("time_limit_now:", time_limit_now)
        for detection_limit_now in [0.5, 0.4, 0.3, 0.2, 0.1]:  # detection_limit_set
            print("detection_limit_now:", detection_limit_now)
            for vehicle_size_now in [8, 7, 6, 5, 4, 3, 2]:  # vehicle_set:
                print("vehicle_size_now:", vehicle_size_now)
                user_inputs.time_limit_now = time_limit_now
                user_inputs.detection_limit_now = detection_limit_now
                user_inputs.vehicle_size_now = vehicle_size_now
                mip_inputs = mip_setup.InputsSetup(user_inputs)
                mip_solve.mathematical_model_solve(mip_inputs)






# back up code

# fire_prone_node_list = [3, 5, 6, 7, 8, 9]  # for 3x3 instance
# fire_prone_node_list = [2, 3, 4, 6, 7, 8, 10, 11, 14, 15, 18, 19, 20, 22, 23, 24, 25]  # for 5x5 instance
   # run_result["number_of_initial_fires"] = len(i)
        # run_result["initial_fire_node_IDs"] = ','.join(map(str, i))
        # run_result.to_csv(writer_file_name, mode="a", index=False, header=False)
