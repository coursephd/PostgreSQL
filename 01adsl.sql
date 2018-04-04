
/*=================================================================*/
/* Execute the code in following manner;                           */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/01adsl.sql; */
/*=================================================================*/

/* Create demog table */
create temp table temp0pat_demog as
select distinct mr_no as mrno, patient_gender, patient_city, patient_state, dateofbirth, country, oldmrno, remarks, death_date
from iaim.patient_details;

/* Create base table */
create temp table temp1doc_cons as
select distinct mr_no, patient_id, consultation_id as consult_id, doctor_name, date(visited_date) as visdate
from iaim.doctor_consultation;

/* Create diagnosis table */
create temp table temp2diag as
select distinct visit_id, id, description, icd_code, diag_type, doctor_id, diagnosis_datetime::timestamptz::date as diagdate
from iaim.mrd_diagnosis;

/* patient_prescription = consultation_id */
/* A Subset is required  for presc_type   */
create temp table temp3pat_presc as
select patient_presc_id, consultation_id, presc_type, status, date(prescribed_date) as dateonly
from iaim.patient_prescription
where presc_type in ('Medicine')
order by patient_presc_id, consultation_id;

/* patient_medicine_prescriptions = medicine_id */
create temp table temp4pat_med as
select medicine_id, op_medicine_pres_id, duration, duration_units, mod_time::timestamptz::date as prescdate, frequency, medicine_quantity, medicine_remarks
from iaim.patient_medicine_prescriptions
order by medicine_id, op_medicine_pres_id;

/* Full join temp0pat_demog and temp1doc_cons */
create temp table temp10 as
select temp0pat_demog.*, temp1doc_cons.*
from temp0pat_demog
full join temp1doc_cons on
temp0pat_demog.mrno = temp1doc_cons.mr_no;

/* Full join temp10 and temp2diag on temp10.patient_id and temp2diag.visit_id */
create temp table temp20 as
select temp10.*, temp2diag.*
from temp10
full join temp2diag on
temp10.patient_id = temp2diag.visit_id;
 
/* Full join temp20 and temp3pat_presc on temp20.consult_id and temp3pat_presc.consultation_id */
create temp table temp30 as
select temp20.*, temp3pat_presc.*
from temp20
full join temp3pat_presc on
temp20.consult_id = temp3pat_presc.consultation_id;

/* Full join temp30 and temp4pat_med on temp30.patient_presc_id and temp4pat_med.op_medicine_pres_id */
create table temp40 as
select temp30.*, temp4pat_med.*
from temp30
full join temp4pat_med on
temp30.patient_presc_id = temp4pat_med.op_medicine_pres_id;

\copy temp40 TO 'd:/hospital_data/ProgresSQL/data_chk/01adsl.csv' CSV HEADER DELIMITER ',';

  