import datarobot as dr
import pandas as pd
import numpy as np

print("# Loading data...")
train = pd.read_csv('numerai_training_data.csv')
tournament = pd.read_csv('numerai_tournament_data.csv')

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

targets = ['target_bernie', 'target_elizabeth', 'target_jordan',
       'target_ken', 'target_charles', 'target_frank', 'target_hillary']

options = dr.AdvancedOptions( accuracy_optimized_mb = True )

for targ in targets:
    print("RUNNING TARGET ", targ)
    temp_list = [keep_cols, targ]
    df_temp = train.loc[:,keep_cols].copy()
    project = dr.Project.create( sourcedata = df_temp)
    project.set_target( target = targ, metric = 'AUC', advanced_options = options )
    project.set_worker_count(20)
    project.wait_for_autopilot()
    # SCORE THE TOURNAMENT DATA AND WRITE TO DISK
    dataset = project.upload_dataset(tournament)
    model = dr.models.ModelRecommendation.get( project.id ).get_model()
    pred_job = model.request_predictions(dataset.id)
    filename = targ + "_preds.csv"
    preds = dr.models.predict_job.wait_for_async_predictions(project.id, predict_job_id=pred_job.id, max_wait=600)

tournament['probability_bernie'] = preds['positive_probability']

df_test = tournament.loc[:,['id', 'probability_bernie']]

df_test.to_csv('bernie_preds.csv', header=True, index=False)        



targ = 'target_hillary'
temp_list = [keep_cols, targ]
df_temp = train.loc[:,keep_cols].copy()
project = dr.Project.create( sourcedata = df_temp)
project.set_target( target = targ, metric = 'AUC', advanced_options = options )
project.set_worker_count(20)
project.wait_for_autopilot()
dataset = project.upload_dataset(tournament)
model = dr.models.ModelRecommendation.get( project.id ).get_model()
pred_job = model.request_predictions(dataset.id)
 
preds = dr.models.predict_job.wait_for_async_predictions(project.id, predict_job_id=pred_job.id, max_wait=600)

tournament['probability_bernie'] = preds['positive_probability']

df_test = tournament.loc[:,['id', 'probability_bernie']]

df_test.to_csv('bernie_preds.csv', header=True, index=False)



project_list = ['5cf4cecff930e26cf4c18c06', '']
targets = ['charles', '']
