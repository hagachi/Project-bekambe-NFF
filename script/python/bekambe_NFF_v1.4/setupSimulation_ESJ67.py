# -*- coding: utf-8 -*-
"""
Created on 17/11/06
"""

import os
import subprocess
import datetime
import numpy as np
import pandas as pd
import xlrd
import shutil
# from PIL import Image # pillow library is required
import rasterio


def select_ecoregions(climdata):
  newdata = pd.DataFrame(climdata[1:, :], columns=climdata[0, :])
  eco_list = []
  for cname in newdata.columns:
    if cname in ['eco{}'.format(name) for name in [eco_unique_vals, 201, 202, 203]]:
      eco_list.append(True)
    elif cname == '':
      eco_list.append(True)
    else:
      eco_list.append(False)
  newdata = newdata.loc[:, eco_list]
  return newdata.columns.values, newdata.values


def make_climdata(climname, eco_unique_vals):
    fname = "../../default_files/climate_data.xlsx"
    BASE_INTERCEPT = 0.024
    AtmosNslope = 0.0058
    varnames = ['prcp', 'Tmax', 'Tmin', 'Ndep']
    for sim_time in ['historical', climname.split('_')[1]]:
      with open('./input_MRI-CGCM3_{}.csv'.format(sim_time), 'w') as f:
        for varname in varnames:
          if varname == 'Ndep': # N-deposition
              out_dat = prcp[:, 0].reshape(prcp.shape[0], 1)
              FERTILIZED = np.array([0, 0, 0, 0, 3.33, 3.33, 0, 3.33, 0, 0, 0, 0]).reshape(12, 1) + BASE_INTERCEPT
              NOT_FERTILIZED = np.ones(shape=(12, 1)) * BASE_INTERCEPT
              for eco_iter in range(len(eco_unique_vals)):
                  # SEE https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession/blob/75812f1fc65013bcfac80ee91cab344168383c9f/src/Main.cs
                  if sim_time == 'historical':
                      if eco_iter == 0: # 森林エコリージョン
                          AtmosNintercept = NOT_FERTILIZED
                      else:
                          AtmosNintercept = FERTILIZED
                  else:
                      # 12か月分のfertilizeデータセットを、放棄されるまで繰り返す
                      if eco_iter == 0: # 森林エコリージョン
                          AtmosNintercept = np.tile(NOT_FERTILIZED, reps=(duration, 1))
                      else: # 牧草地エコリージョン
                          ndep_fertilize = np.tile(FERTILIZED, reps=(min(eco_unique_vals[eco_iter] % 1000, duration), 1))
                          ndep_nofertilize = np.tile(NOT_FERTILIZED, reps=(max(int(duration - eco_unique_vals[eco_iter] % 1000), 0), 1))
                          AtmosNintercept = np.concatenate([ndep_fertilize, ndep_nofertilize], axis=0)
                  # Compute n-deposition for each month
                  ndep = prcp[:, 1].reshape(out_dat.shape[0], 1) * 0.1 * AtmosNslope + AtmosNintercept # convert prcp from mm to cm
                  out_dat = np.concatenate([out_dat, ndep], axis=1)

          else: # Other climate variables
              # Read climate data from xlsx file
              dat = pd.read_excel(fname, sheet_name='{}_{}'.format(sim_time, varname))
              dat = np.asarray(dat)
              duration = int(dat.shape[0]/12)

              # store precipitation for Ndep calculation
              if varname == 'prcp':
                  prcp = dat

              # Mutate columns for each ecoregion
              out_dat = dat[:, 0].reshape(dat.shape[0], 1)
              for _ in range(len(eco_unique_vals)):
                  out_dat = np.concatenate([out_dat, dat[:, 1].reshape(dat.shape[0], 1)], axis=1)

          # insert var and stdev
          for _ in range(len(eco_unique_vals) * 2):
              out_dat = np.concatenate([out_dat, np.zeros(shape=(out_dat.shape[0], 1))], axis=1)

          # Write data
          if varname == 'prcp':
            f.write('#prcp\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(mm/month)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(mm/month^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(mm/month)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Tmax':
            f.write('#Tmax\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(degC)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(degC^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(degC)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Tmin':
            f.write('#Tmin\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(degC)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(degC^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(degC)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))
          elif varname == 'Ndep':
            f.write('#Ndep\n')
            buf = np.tile(eco_unique_vals, reps=(1, 3)).reshape(len(eco_unique_vals)*3,).tolist()
            buf = [str(eco) for eco in buf]
            f.write(',eco{}\n'.format(',eco'.join(buf)))
            buf = ['TIMESTEP']
            buf.extend(['MEAN(gN/month)']*len(eco_unique_vals))
            buf.extend(['VARIANCE(gN/month^2)']*len(eco_unique_vals))
            buf.extend(['STD_DEV(gN/month)']*len(eco_unique_vals))
            f.write('{}\n'.format(','.join(buf)))

          for _ in range(out_dat.shape[0]):
            buf = [str(val) for val in out_dat[_, :]]
            f.write('{}\n'.format(','.join(buf)))


def get_mng_eco_code(paarea, solrate):

    with rasterio.open(os.path.join(rootdir, 'input', '191223_mngMap_BaU_area{}_sol{:.1f}.tif'.format(paarea, solrate))) as f:
        mngras = f.read(1)

    with rasterio.open(os.path.join(rootdir, 'input', 'ecoregions_bekambe_BaU_{}_v1.0.tif'.format(paarea))) as f:
        ecoras = f.read(1)

    # Get unique code for each pastureland utilization IDs
    mng_unique_vals = np.unique(mngras)
    eco_unique_vals = np.unique(ecoras)
    eco_unique_vals = eco_unique_vals[eco_unique_vals > 0]
    # print(eco_unique_vals)
    # print(mng_unique_vals)

    eco4pasture = np.sort(eco_unique_vals[eco_unique_vals >= 1000])
    mng4biomass = np.sort(mng_unique_vals[(mng_unique_vals >= 1000) & (mng_unique_vals < 2000)])
    mng4riparian = np.sort(mng_unique_vals[(mng_unique_vals >= 2000) & (mng_unique_vals < 3000)])
    mng4solar = np.sort(mng_unique_vals[(mng_unique_vals >= 3000) & (mng_unique_vals < 4000)])
    mng4pasture = np.sort(mng_unique_vals[mng_unique_vals >= 4000])
    return eco_unique_vals, eco4pasture, mng4biomass, mng4riparian, mng4solar, mng4pasture


def set_ecoregiontxt(eco4pasture):
    shutil.copy('../../default_files/ecoregions_bekambe.txt', './ecoregions_bekambe.txt')
    with open("ecoregions_bekambe.txt", "a", encoding = "utf-8") as f:
        f.write('\n>> Setting for pasture land\n')
        for mapcode in eco4pasture:
            f.write('\tyes\t{}\teco{}\t"pastureland"\n'.format(mapcode, mapcode))


def set_necn_successsiontxt(climname):
    oldClimateConfigName = "ClimateConfigFile\n"
    fertilizeMode = "A"
    newClimateConfigName = "ClimateConfigFile           ../../ini/climate-generator_%s_%s.txt\n" % (climname, fertilizeMode)

    oldNECNIniFile = open("../../default_files/NECN-succession_bekambe.txt", encoding = "utf-8")
    newNECNIniFile = open("NECN-succession_bekambe.txt", "w", encoding = "utf-8")

    for s in oldNECNIniFile:
        newNECNIniFile.write(s.replace(oldClimateConfigName, newClimateConfigName))

    oldNECNIniFile.close()
    newNECNIniFile.close()


def set_scenariotxt(paarea):
    oldEcoregionsMapName = "EcoregionsMap replace here\n"
    newEcoregionsMapName = "EcoregionsMap               ../../input/ecoregions_bekambe_BaU_%s_v1.0.tif\n" % (paarea)

    oldScenarioFile = open("../../default_files/scenario.txt", encoding = "utf-8")
    newScenarioFile = open("scenario.txt", "w", encoding = "utf-8")

    for s in oldScenarioFile:
        if s == oldEcoregionsMapName:
            newScenarioFile.write(s.replace(oldEcoregionsMapName, newEcoregionsMapName))
        else:
            newScenarioFile.write(s)

    oldScenarioFile.close()
    newScenarioFile.close()


def return_rotation(cutmethod, forestage, plant_spp):
    # list内のラベルが増えればif文の書き方が変わる
    #　rotationしないaroundも決めるならそれも追加する
    if cutmethod == 'clear':
        if forestage == 'normal':
            if plant_spp == 'conifer':
                national_rotation = 30
                pref_rotation = 50
            else:
                national_rotation = 40
                pref_rotation = 40
        else:
            if plant_spp == 'conifer':
                national_rotation = 60
                pref_rotation = 100
            else:
                national_rotation = 80
                pref_rotation = 80
    else:
        national_rotation = 15
        pref_rotation = 15
    return national_rotation, pref_rotation


def write_forestmanagement_table(f, hmode, cutmethod, forestage, plantspp, national_rotation, pref_rotation):
    # グリッドサーチ対象のエリアのharvest implementation tableの書き込み
    endyear = 85
    if hmode == "around":
        endyear = 15
    # National forest --------
    for pcode in national_pcodes:
        # aroundではrotationはないがどうやって年間施業面積を決めるか。continueのbroadと同じにした。
        # 場合分けの書き方
        if plantspp == 'conifer':
            # 以下の施業の名前をaroundとcontinueに対応させる
            f.write('{} {}_{}_{}_{}_national {:.1f}% 1 {}\n'.format(pcode, hmode, cutmethod, forestage, plantspp, 100*1/national_rotation, endyear))
        else:
            f.write('{} {}_{}_{}_{} {:.1f}% 1 {}\n'.format(pcode, hmode, cutmethod, forestage, plantspp, 100*1/national_rotation, endyear))
        if cutmethod == 'clear':
            f.write('{} {}_thinning_{} {:.1f}% 1 {}\n'.format(pcode, hmode, plantspp, 100*1/(national_rotation/4), endyear)) # rotation periodごとに4回間伐する


    # prifecture forest --------
    for pcode in pref_pcodes:
        if plantspp == 'conifer':
            f.write('{} {}_{}_{}_{}_pref {:.1f}% 1 {}\n'.format(pcode, hmode, cutmethod, forestage, plantspp, 100*1/pref_rotation, endyear))
        else:
            f.write('{} {}_{}_{}_{} {:.1f}% 1 {}\n'.format(pcode, hmode, cutmethod, forestage, plantspp, 100*1/pref_rotation, endyear))
        if cutmethod == 'clear':
            f.write('{} {}_thinning_{} {:.1f}% 1 {}\n'.format(pcode, hmode, plantspp, 100*1/(pref_rotation/4), endyear)) # rotation periodごとに4回間伐する


def write_pasturemanagement_table(f, hmode, paarea, mng4riparian, mng4solar, mng4biomass, ispasplant, mng4pasture):
    # 牧草管理のharvest implementation tableの書き込み -----------------------
    # リストがあるかどうかを毎回チェックすること
    # 1. いつまで管理するかを決める
    # 2. 追加の管理方法を書き込む
    # の順に、それぞれ設定せよ

    # for Riparian zone =======================================================
    if len(mng4riparian) > 0:
        f.write('\n>> Setting for natural riparian zone\n')
        for mng4riparian_iter in range(len(mng4riparian)):
            change_year = min(85, mng4riparian_iter + 1)
            if paarea == 0:
                change_year = 85
            mng_id = mng4riparian[mng4riparian_iter]
            # 1.1 河畔林に転用する牧草地
            f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))
            # 2.1 河畔林は放置する


    # for Solar PV area =======================================================
    if len(mng4solar) > 0:
        f.write('\n>> Setting for solar power energy\n')
        for mng4solar_iter in range(len(mng4solar)):
            change_year = min(85, mng4solar_iter + 1)
            if paarea == 0:
                change_year = 85
            mng_id = mng4solar[mng4solar_iter]
            # 1.2 太陽光パネルに転用する牧草地
            f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))
            # 2.2 太陽光パネルの設置用地は全部刈り取る
            if paarea > 0:
                if change_year < 85:
                    f.write('{} stop_and_solar 100% {} 85\n'.format(mng_id, change_year + 1))


    # for biomass energy area =================================================
    if len(mng4biomass) > 0:
        f.write('\n>> Setting for biomass energy\n')
        for mng4biomass_iter in range(len(mng4biomass)):
            change_year = min(85, mng4biomass_iter + 1)
            if paarea == 0:
                change_year = 85
            mng_id = mng4biomass[mng4biomass_iter]
            # 1.3 バイオマス利用に転用する牧草地
            f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, change_year))
            # 2.3 バイオマス利用するなら、定着しやすいであろうカンバを定期的に伐採する
            if paarea > 0 and change_year + 1 + BETULA_AGE < 86:
                # plant or not plantで場合分け
                # 切る樹種も違う
                if change_year < 85:
                    if ispasplant == "plant":
                        f.write('{} abandonment_and_plant_broad 100% {} {}\n'.format(mng_id, change_year + 1, change_year + 1))
                        if hmode == 'continue':
                            f.write('{} quercris_ClearCutting_abandoned 100% {} 85\n'.format(mng_id, change_year + 1 + QUERCUS_AGE))
                    else:
                        if hmode == 'continue':
                            f.write('{} betuplat_ClearCutting_abandoned 100% {} 85\n'.format(mng_id, change_year + 1 + BETULA_AGE))


    # for managing pastureland ================================================
    if len(mng4pasture) > 0:
        f.write('\n>> Setting for managing pastureland\n')
        mng_id = mng4pasture[0]
        f.write('{} Agriculture 100% 0 {}\n'.format(mng_id, 85))


def set_biomassharvesttxt(paarea, solrate, hmode, cutmethod, forestage, plantspp, national_rotation, pref_rotation,
                          mng4riparian, mng4solar, mng4biomass, ispasplant, mng4pasture):
    # 元のBiomass harvestを開いて、ファイルの中身を取得
    with open('../../default_files/BiomassHarvest.txt', 'r', encoding = "utf-8") as f:
        lines = f.readlines()

    # scenario.txtと同じ階層に、設定ファイルを保存。ファイル内をシナリオに合わせて修正。
    with open("./BiomassHarvest.txt", "w", encoding = "utf-8") as f:
        # ひな型をコピーする
        for line in lines:
            if line == 'ManagementAreas	replacehere\n':
                f.write('ManagementAreas	../../input/191223_mngMap_BaU_area{}_sol{:.1f}.tif\n'.format(paarea, solrate))
            else:
                f.write(line)

        # For Forest management ===============================================
        write_forestmanagement_table(f, hmode, cutmethod, forestage, plantspp, national_rotation, pref_rotation)

        # For Pasture management ==============================================
        write_pasturemanagement_table(f, hmode, paarea, mng4riparian, mng4solar, mng4biomass, ispasplant, mng4pasture)

        # output setting
        f.write('\n>> Outputs\n')
        f.write('PrescriptionMaps    OutputMaps/harvest/prescripts-{timestep}.img\n')
        f.write('BiomassMaps         OutputMaps/harvest/biomass-removed-{timestep}.img\n')
        f.write('EventLog            biomass-harvest-event-test-log.csv\n')
        f.write('SummaryLog	    	biomass-harvest-summary-log.csv\n')


def prep_inifiles(paarea, biomrate, ispasplant, solrate,
                  cutmethod, forestage, plantspp, climname,
                  eco_unique_vals, eco4pasture, mng4biomass, mng4riparian, mng4solar, mng4pasture):
    # Replace 0: ecoregion.txt ================================
    try:
        set_ecoregiontxt(eco4pasture)
    except:
        print('failed to make ecoregions_bekambe.txt')


    # Create climate data for the case ========================
    # 農地エコリージョンの数が可変なので、注意したい。
    try:
        make_climdata(climname, eco_unique_vals)
    except:
        print('failed to make climate data')


    # Replace1: climate configfile in NECN-succession.txt ======
    try:
        set_necn_successsiontxt(climname)
    except:
        print("Climate config file copy error")


    # Replace2: ecoregion map name in scenario.txt ============
    try:
        set_scenariotxt(paarea)
    except:
      print("scenario.txt file copy error")


    # Replace 3: Harvest
    # Set rotation period for each case -----
    national_rotation, pref_rotation = return_rotation(cutmethod, forestage, plantspp)
    try:
        # TODO: Continuous or Aroundで変える
        set_biomassharvesttxt(paarea, solrate, hmode, cutmethod, forestage, plantspp, national_rotation, pref_rotation,
                              mng4riparian, mng4solar, mng4biomass, ispasplant, mng4pasture)
    except:
        print('BiomassHarvest.txt file error')


def set_forest_management(paarea, biomrate, ispasplant, solrate):
    for cutmethod in cutmethods:
        for forestage in forestages:
            for plantspp in plantspps:
                # TODO: Continuous or Aroundで変える********************************************
                mngname = '{}_a{}_s{:.1f}_{}_{}_{}_{}'.format(hmode, paarea, solrate, ispasplant, cutmethod, forestage, plantspp)
                print(mngname)
                mngdir = os.path.join(rootdir, mngname)
                if not os.path.isdir(mngdir):
                    os.mkdir(mngdir)

                # Check the management and ecoregion raster to get management and ecoregion codes
                eco_unique_vals, eco4pasture, mng4biomass, mng4riparian, mng4solar, mng4pasture = get_mng_eco_code(paarea, solrate)

                for climname in climnames:
                    climdir = os.path.join(mngdir, climname)
                    try:
                        if not os.path.isdir(climdir):
                            os.mkdir(climdir)
                        os.chdir(climdir)
                    except:
                        print("failed to make climate directory")

                    # prepare ini-files
                    # TODO: Continuous or Aroundで変える
                    prep_inifiles(paarea, biomrate, ispasplant, solrate,
                                  cutmethod, forestage, plantspp, climname,
                                  eco_unique_vals, eco4pasture, mng4biomass, mng4riparian, mng4solar, mng4pasture)

                # Delete objects
                del eco4pasture
                del mng4biomass
                del mng4riparian
                del mng4solar
                del mng4pasture


def set_pasture_continue_management():
    for paarea in paareas:
        for biomrate in biomrates:
            # Check combinations between biom-solar-pa_areas
            if paarea == 0:
                if biomrate > 0: # those cases show the same result as 0-0 case
                    break
                biomrate = round(biomrate * 0.1, 1)
                solrate = 0
                ispasplant = 'notplant'
                set_forest_management(paarea, biomrate, ispasplant, solrate)
                break
            else:
                biomrate = round(biomrate * 0.1, 1)
                solrate = round(1 - biomrate, 1)
                if biomrate == 0.0:
                    ispasplant = 'notplant'
                    set_forest_management(paarea, biomrate, ispasplant, solrate)
                else:
                    for ispasplant in ispasplants:
                        # Forest management =============================================
                        set_forest_management(paarea, biomrate, ispasplant, solrate)

def set_pasture_around_management():
    for paarea in paareas:
        biomrate = round(10 * 0.1, 1)
        solrate = 0
        if paarea == 0:
            ispasplant = 'notplant'
            set_forest_management(paarea, biomrate, ispasplant, solrate)
        else:
            for ispasplant in ispasplants:
                # Forest management =============================================
                set_forest_management(paarea, biomrate, ispasplant, solrate)


if __name__ == "__main__":
    # set variables
    mngnames = ['BaU']
    climnames = ["MRI_rcp26", "MRI_rcp85"]
    # climnames = ["MRI_rcp26"]
    # climnames = ["MRI_rcp85"]
    BETULA_AGE = 25
    QUERCUS_AGE = 20

    # prescription code
    national_pcodes = [1,4]
    pref_pcodes = [5,6,7,10,11,12,21]
    all_pcodes = [1,4,5,6,7,10,11,12,21]

    # set root directory
    # os.chdir(os.path.dirname(os.path.abspath(__file__)))
    # rootdir = os.path.abspath("../../")
    rootdir = r"F:\210113_bekambe_NFF_v1.5"
    print('Root directory: %s'%rootdir)

    # setting for NS and NC =============================================
    hmode = "continue"
    # paareas = [223, 201, 178, 156, 134, 112, 89, 67, 45, 22, 0]
    paareas = [112]
    biomrates = [0, 5, 10]
    ispasplants = ['plant', 'notplant']
    cutmethods = ["clear", "selective"]
    forestages = ["normal", "long"]
    plantspps = ["conifer", "broad"]
    set_pasture_continue_management()

    # For NN scenario =================================================
    hmode = "around"
    paareas = [112]
    biomrates = [10]
    ispasplants = ['plant', 'notplant']
    cutmethods = ["selective"]
    forestages = ["normal", "long"]
    plantspps = ["broad", "natural"]
    # TODO: 放棄地で収穫しない
    set_pasture_around_management()
