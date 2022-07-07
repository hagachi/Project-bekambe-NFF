# -*- coding: utf-8 -*-
"""
Created on Tue Dec 24 09:41:26 2019

@author: GE
"""

import os
import subprocess
from joblib import Parallel, delayed
import datetime
import shutil

# Define functions =-=====================================
# Run LANDIS-II model function ----
def run_landisii(scenario_path):
    os.chdir(scenario_path)
    print('Model run started: {}'.format(scenario_path))
    command = 'landis-ii-7 scenario.txt | echo off'
    try:
        subprocess.call(command, shell = True)
        with open('Landis-log.txt', 'r') as f:
            lines = f.readlines()
        if "Model run is complete" in lines[-1]:
            print('Model run finished: {}'.format(scenario_path))
            shutil.copy('Landis-log.txt', os.path.join(db_path, "Success" + scenario_path.split("\\")[-2] + '_log.txt'))
        else:
            print('Error! See Landis-log.txt in {}'.format(scenario_path))
            shutil.copy('Landis-log.txt', os.path.join(db_path, "Error" + scenario_path.split("\\")[-2] + '_log.txt'))
    except:
        print('Error in processing {} ...'.format(scenario_path))
        with open(os.path.join(db_path, "Error_" + scenario_path.split("\\")[-2] + '_log.txt'), "w") as f:
            f.write("Error! LANDIS could not started and No log files were created.")

# Parallel control ------
def usejoblib(job, path_list):
    Parallel(n_jobs=job)([delayed(run_landisii)(path) for path in path_list])

# Main ===================================================
if __name__ == "__main__":
    CORE_NUMBER = 5 # NUMBER OF CORES FOR MULTIPROCESSING
    root_path = r'C:\Users\GE\Documents\210113_bekambe_NFF_v1.5'
    db_path = os.path.join(r'C:\Users\GE\Dropbox\landis_group\pj01_S15-bekambe\logs', datetime.date.today().strftime('%Y-%m-%d'))
    if not os.path.isdir(db_path):
        os.mkdir(db_path)

    os.chdir(root_path)
    mng_list = [path for path in os.listdir() if os.path.isdir(path) and (path.startswith('around') or path.startswith('continue'))]
    scenario_path = []
    for rcp_name in ['MRI_rcp26', 'MRI_rcp85']:
    # for rcp_name in ['MRI_rcp85']:
        for mng_name in mng_list:
            scenario_path.append(os.path.join(root_path, mng_name, rcp_name))

    usejoblib(CORE_NUMBER, scenario_path)
