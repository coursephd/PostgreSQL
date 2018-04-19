
/* 19th April 2018 */
/* SQL version with UNION of data */

/*============================================================================*/
/* Execute the code in following manner;                                      */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/100_adsl_sqlpart.sql;  */
/*============================================================================*/

drop table if exists 
temp0pat_demog, temp1pat_reg, temp1doc_cons, temp2reg_cons, temp2diag, temp3pat_presc, temp4pat_med,
temp20, temp30, temp30_1, temp30_5, temp100ip, temp350, temp100ser, temp100ser2, temp360,
base01_op0, base01_op, base01_ip, base01_ser, base_all ;

/* Create demog table */
create temp table temp0pat_demog as
select distinct mr_no as mrno, patient_gender, patient_city, patient_state, dateofbirth, country, /*oldmrno, remarks,*/ death_date
from iaim.patient_details;

create temp table temp1pat_reg as
select mr_no, patient_id, visit_type, reg_date, bed_type, dept_name, admitted_dept, main_visit_id
from iaim.patient_registration
order by mr_no, patient_id;

create temp table temp1doc_cons as
select distinct mr_no as con_mrno, patient_id as con_patient_id, consultation_id as consult_id, doctor_name, date(visited_date) as visdate
from iaim.doctor_consultation
order by mr_no, patient_id;

create temp table temp2reg_cons as
select temp1pat_reg.*, temp1doc_cons.*
from temp1pat_reg
full join temp1doc_cons on
temp1pat_reg.mr_no = temp1doc_cons.con_mrno and temp1pat_reg.patient_id = temp1doc_cons.con_patient_id;

/* Create diagnosis table */
create temp table temp2diag as
select distinct visit_id, id, description, icd_code, diag_type, doctor_id, diagnosis_datetime::timestamptz::date as diagdate
from iaim.mrd_diagnosis;

/* Full join temp10 and temp2diag on temp10.patient_id and temp2diag.visit_id */
create temp table temp20 as
select temp2reg_cons.*, temp2diag.*
from temp2reg_cons
full join temp2diag on
temp2reg_cons.patient_id = temp2diag.visit_id;

/* patient_prescription = consultation_id */
/* A Subset is required  for presc_type   */
create temp table temp3pat_presc as
select patient_presc_id, consultation_id, presc_type, status, date(prescribed_date) as dateonly
from iaim.patient_prescription
/*where presc_type in ('Medicine') */
order by patient_presc_id, consultation_id;

create temp table temp30 as
select temp20.*, temp0pat_demog.*
from temp20
full join temp0pat_demog on
temp20.mr_no = temp0pat_demog.mrno;

create temp table temp30_1 as
select temp30.*, temp3pat_presc.*
from temp30
full join temp3pat_presc on
temp30.consult_id = temp3pat_presc.consultation_id;

create temp table temp30_5 as
select mr_no, patient_id, patient_gender, patient_city, patient_state, dateofbirth, country, death_date, 
consult_id, description, icd_code, diag_type, diagdate, patient_presc_id
from temp30_1;

/* patient_medicine_prescriptions = medicine_id */
create temp table temp4pat_med as
select medicine_id as cat_id, 
op_medicine_pres_id, 
duration, 
duration_units, 
mod_time::timestamptz::date as prescdate, 
frequency, 
medicine_quantity as quantity, 
medicine_remarks as remarks
from iaim.patient_medicine_prescriptions
order by medicine_id, op_medicine_pres_id;

/* BASE 1 data for the OP medication */

create temp table base01_op as
select temp30_5.*, temp4pat_med.*
from temp4pat_med
left join temp30_5 on
temp30_5.patient_presc_id = temp4pat_med.op_medicine_pres_id
order by mr_no, patient_id;

/* IP medications */
create temp table temp100ip as
select 
prescription_id as consultation_id, 
patient_id as ippatient_id, 
doctor_id, 
prescription_date::timestamptz::date as prescdate,
presc_type, 
item_id as cat_id, 
item_name, 
med_dosage as quantity, 
med_route, 
med_form_id, 
generic_code, 
remarks, 
recurrence_daily_id as frequency
from iaim.ip_prescription
order by patient_id;

create temp table base01_ip as
select temp30_5.*, temp100ip.*
from temp100ip
left join temp30_5 on
temp30_5.patient_id = temp100ip.ippatient_id;

/* Services Create Base01_ser */

create temp table base01_ser as
select mr_no, 
patient_id, 
service_id as cat_id, 
presc_date::timestamptz::date as prescdate, 
conducted, 
conductedby, 
conducteddate::timestamptz::date as sercond_date, 
prescription_id as consultation_id
from iaim.services_prescribed;

create temp table services as
select service_id as medicine_id, service_name as medicine_name 
from iaim.services;

create temp table med as
select distinct medicine_name, medicine_id 
from iaim.medicine_sales_view;

\copy temp0pat_demog TO 'd:/hospital_data/ProgresSQL/source/pat_info.csv' CSV HEADER DELIMITER ',';
\copy temp30_5 TO 'd:/hospital_data/ProgresSQL/source/pat_diag_vis.csv' CSV HEADER DELIMITER ',';
\copy base01_ip TO 'd:/hospital_data/ProgresSQL/source/base01_ip.csv' CSV HEADER DELIMITER ',';
\copy base01_op TO 'd:/hospital_data/ProgresSQL/source/base01_op.csv' CSV HEADER DELIMITER ',';
\copy base01_ser TO 'd:/hospital_data/ProgresSQL/source/base01_ser.csv' CSV HEADER DELIMITER ',';

\copy services TO 'd:/hospital_data/ProgresSQL/source/services.csv' CSV HEADER DELIMITER ',';
\copy med TO 'd:/hospital_data/ProgresSQL/source/med.csv' CSV HEADER DELIMITER ',';

/*=========================================*/
/* End of program                          */
/*=========================================*/
