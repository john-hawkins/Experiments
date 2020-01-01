import datarobot as dr
import pandas as pd
import os
import numpy as np
import zipfile
from zipfile import ZipFile

print("# Loading data...")
train = pd.read_csv('data/numerai_training_data.csv')
tournament = pd.read_csv('data/numerai_tournament_data.csv')

train['epoch'] = train.era.str[3:]
tournament['epoch'] = tournament.era.str[3:]

keep_cols = ['feature1', 'feature2', 'feature3',
       'feature4', 'feature5', 'feature6', 'feature7', 'feature8', 'feature9',
       'feature10', 'feature11', 'feature12', 'feature13', 'feature14',
       'feature15', 'feature16', 'feature17', 'feature18', 'feature19',
       'feature20', 'feature21', 'feature22', 'feature23', 'feature24',
       'feature25', 'feature26', 'feature27', 'feature28', 'feature29',
       'feature30', 'feature31', 'feature32', 'feature33', 'feature34',
       'feature35', 'feature36', 'feature37', 'feature38', 'feature39',
       'feature40', 'feature41', 'feature42', 'feature43', 'feature44',
       'feature45', 'feature46', 'feature47', 'feature48', 'feature49',
       'feature50']

#targets = ['target_bernie', 'target_elizabeth', 'target_jordan',
#       'target_ken', 'target_charles', 'target_frank', 'target_hillary']
#targets = [ 'target_ken', 'target_charles', 'target_frank', 'target_hillary']
targets = [ 'target_charles', 'target_frank', 'target_hillary']

options = dr.AdvancedOptions( accuracy_optimized_mb = True )

temp_csv = 'temp_file.csv'
temp_zip = 'temp_file.zip'

for targ in targets:
    if os.path.exists(temp_csv):
        os.remove(temp_csv)
    if os.path.exists(temp_zip):
        os.remove(temp_zip)
    print("RUNNING TARGET ", targ)
    temp_list = keep_cols.copy()
    temp_list.append(targ)
    df_temp = train.loc[:,temp_list].copy()
    # WRITE TO DISK AND COMPRESS
    df_temp.to_csv(temp_csv, header=True, index=False)
    with ZipFile(temp_zip,'w', zipfile.ZIP_DEFLATED) as zip: 
        zip.write(temp_csv)
    project = dr.Project.create( sourcedata = temp_zip, project_name=targ)
    project.set_target( target = targ, metric = 'AUC', advanced_options = options )
    project.set_worker_count(20)
    project.wait_for_autopilot()
    # SCORE THE TOURNAMENT DATA AND WRITE TO DISK
    dataset = project.upload_dataset(tournament)
    model = dr.models.ModelRecommendation.get( project.id ).get_model()
    pred_job = model.request_predictions(dataset.id)
    filename = targ + "_preds.csv"
    preds = dr.models.predict_job.wait_for_async_predictions(project.id, predict_job_id=pred_job.id, max_wait=600)
    just_name = targ.split('_')[1]
    new_col_name = 'probability_' + just_name 
    tournament[new_col_name] = preds['positive_probability']
    df_test = tournament.loc[:,['id', new_col_name]]
    df_test.to_csv(new_col_name +'.csv', header=True, index=False)        


