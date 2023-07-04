import gurobipy as gp
import pandas as pd
import os
from ast import literal_eval

# define setup classes
class InputsSetup:
    def __init__(self, user_inputs):

        # read problem input
        self.directory = user_inputs.directory
        self.target_df = user_inputs.target_df.copy()
        self.trajectory_df = user_inputs.trajectory_df.copy()
        self.parameters_df = user_inputs.parameters_df.copy()
        self.experiment_mode = self.parameters_df.loc["mode", "value"]

        # problem parameters
        self.n_nodes = self.parameters_df.loc["n_nodes", "value"]
        self.n_targets = self.parameters_df.loc["n_targets", "value"]
        self.node_list = list(range(1, int(self.n_nodes) + 1))
        self.target_list = list(range(1, int(self.n_targets) + 1))
        self.n_revisits = self.parameters_df.loc["n_revisits", "value"]
        self.revisit_list = list(range(1, int(self.n_revisits) + 1))
        self.n_time_steps = self.parameters_df.loc["n_time_steps", "value"]
        self.time_step_list = list(range(1, int(self.n_time_steps) + 1))

        if self.experiment_mode == "multi_objective":
           self.n_vehicles = user_inputs.vehicle_size_now
           self.time_limit = user_inputs.time_limit_now
           self.detection_limit = user_inputs.detection_limit_now
           self.run_start_date = user_inputs.run_start_date
        else:
            self.n_vehicles = self.parameters_df.loc["n_vehicles", "value"]
            self.time_limit = self.parameters_df.loc["time_limit", "value"]
            self.detection_limit = self.parameters_df.loc["probability_of_detection_limit", "value"]

        self.vehicle_list = list(range(1, int(self.n_vehicles) + 1))
        self.vehicle_flight_speed = self.parameters_df.loc["vehicle_flight_speed", "value"]
        self.base_node_id = 0

        # define empty lists for classes, i.e. each element of the list will be an element of the corresponding class
        self.nodes_multidict_input = {}  # dictionary of elements of node attribute class
        self.links_multidict_input = {}  # multi-dictionary input for the transportation cost information for each available arc in the network

        # setup links and their distances
        for t in range(1, int(self.n_time_steps) + 1):
            for i in range(len(self.trajectory_df)):
                values = self.trajectory_df.loc[i, :]
                for k in range(1, int(self.n_vehicles) + 1):
                    self.links_multidict_input[t, int(values["from_node"]), int(values["to_node"]), k] = [values["travel_duration"], values["number_of_detections"]]

        # setup multi dictionaries --> if you are unfamiliar with multidict and want to learn about, go to below link
        # https://www.gurobi.com/documentation/8.1/refman/py_multidict.html
        self.links, self.links_durations, self.links_detections = gp.multidict(self.links_multidict_input)

        # setup nodes and their attributes
        for i in range(len(self.target_df)):
            values = self.target_df.loc[i, :]
            for k in range(1, int(self.n_vehicles) + 1):
                self.nodes_multidict_input[int(values["target_id"]), int(values["visit_id"]), k] = [
                    values["search_duration"], values["expected_information"], values["number_of_detections"]]

        self.targets_visits, self.targets_search_duration, self.targets_expected_information, self.targets_detections = gp.multidict(self.nodes_multidict_input)


class UserInputsRead:
    def __init__(self):

        # read problem input
        self.directory = os.path.join('inputs', 'inputs_to_load.xlsx')  # os.getcwd(),
        self.parameters_df = pd.read_excel(self.directory, sheet_name="parameters", index_col=0, engine='openpyxl').dropna(axis=0, how='all').dropna(axis=1, how='all')
        self.target_df = pd.read_excel(self.directory, sheet_name="target_df", engine='openpyxl').dropna(axis=0, how='all').dropna(axis=1, how='all')
        self.trajectory_df = pd.read_excel(self.directory, sheet_name="trajectory_df", engine='openpyxl').dropna(axis=0, how='all').dropna(axis=1, how='all')
