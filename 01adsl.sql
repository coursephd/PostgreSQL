
/*=================================================================*/
/* Execute the code in following manner;                           */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/01adsl.sql; */
/*=================================================================*/

/*=============================*/
/* Updates on 8th April 2018   */
/* NEW ADSL equivalent dataset */
/*=============================*/

drop table if exists temp1pat_reg, temp1doc_cons, temp2reg_cons, temp2diag, temp3pat_presc, temp20, temp30, temp100ip, temp350, temp100ser, temp360;

create table temp1pat_reg as
select mr_no, patient_id, visit_type, reg_date, bed_type, dept_name, admitted_dept, main_visit_id
from iaim.patient_registration
order by mr_no, patient_id;

create table temp1doc_cons as
select distinct mr_no as con_mrno, patient_id as con_patient_id, consultation_id as consult_id, doctor_name, date(visited_date) as visdate
from iaim.doctor_consultation
order by mr_no, patient_id;

create table temp2reg_cons as
select temp1pat_reg.*, temp1doc_cons.*
from temp1pat_reg
full join temp1doc_cons on
temp1pat_reg.mr_no = temp1doc_cons.con_mrno and temp1pat_reg.patient_id = temp1doc_cons.con_patient_id;

/* Create diagnosis table */
create table temp2diag as
select distinct visit_id, id, description, icd_code, diag_type, doctor_id, diagnosis_datetime::timestamptz::date as diagdate
from iaim.mrd_diagnosis;

/* Full join temp10 and temp2diag on temp10.patient_id and temp2diag.visit_id */
create table temp20 as
select temp2reg_cons.*, temp2diag.*
from temp2reg_cons
full join temp2diag on
temp2reg_cons.patient_id = temp2diag.visit_id;

/* patient_prescription = consultation_id */
/* A Subset is required  for presc_type   */
create table temp3pat_presc as
select patient_presc_id, consultation_id, presc_type, status, date(prescribed_date) as dateonly
from iaim.patient_prescription
/*where presc_type in ('Medicine') */
order by patient_presc_id, consultation_id;

create /*temp*/ table temp30 as
select temp20.*, temp3pat_presc.*
from temp20
full join temp3pat_presc on
temp20.consult_id = temp3pat_presc.consultation_id;

/* IP medications */

create table temp100ip as
select prescription_id, patient_id as ippatient_id, doctor_id as ipdoctorid, 
prescription_date::timestamptz::date as ipprescdate,
presc_type as ippresc_type, item_id, item_name, med_dosage, med_route, med_form_id, 
generic_code, remarks as ipremarks, recurrence_daily_id
from iaim.ip_prescription
order by ippatient_id;

create table temp350 as
select temp30.*, temp100ip.*
from temp30
full join temp100ip on
temp30.patient_id = temp100ip.ippatient_id;

/* Services */

create table temp100ser as
select mr_no as ser_mrno, patient_id as ser_patient_id, service_id, doctor_id as serdoctorid, 
presc_date::timestamptz::date as serprescdate, conducted, conductedby, 
conducteddate::timestamptz::date as sercond_date, prescription_id as serprescription_id
from iaim.services_prescribed;

/* This merge creates 1 cror rows */

create table temp360 as
select temp350.*, temp100ser.*
from temp350
full join temp100ser on
temp350.patient_id = temp100ser.ser_patient_id;

\copy temp360 TO 'd:/hospital_data/ProgresSQL/data_chk/temp360.csv' CSV HEADER DELIMITER ',';

/* End of the new version of the program */


drop table if exists temp40, temp310, temp315;

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
create /*temp*/ table temp30 as
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


/*=========================================================================================================*/
/* 7th April 2018 updates                                                                                  */
/* This is the extra addition for IP medcines, Merge the disease history                                   */
/*=========================================================================================================*/

/* Use ip_prescription to get IP medicines */
/* treatment_chart only 33 records */

create temp table temp100adm as
select mr_no as ipmrno, patient_id, date(admit_date) as ipdateonly, isbaby
from iaim.admission
order by mr_no, patient_id;

create temp table temp100ip as
select prescription_id, patient_id as temp_id, doctor_id as ipdoc_id, prescription_date::timestamptz::date as ipprescdate,
presc_type as ippresc_type, item_id, item_name, med_dosage, med_route, med_form_id, generic_code, remarks as ipremarks, recurrence_daily_id
from iaim.ip_prescription;

create temp table temp300 as
select temp100adm.*, temp100ip.*
from temp100adm
full join temp100ip on
temp100adm.patient_id = temp100ip.temp_id;

create /*temp*/ table temp310 as
select * from temp300
where prescription_id > 0;

alter table temp310 drop patient_id;

create table temp315 as
select temp30.*, temp310.*
from temp310
left join temp30 on
temp30.mr_no = temp310.ipmrno and temp30.patient_id = temp310.temp_id;

create table temp320 as
from temp315
where;

\copy temp40 TO 'd:/hospital_data/ProgresSQL/data_chk/01adsl.csv' CSV HEADER DELIMITER ',';
\copy temp315 TO 'd:/hospital_data/ProgresSQL/data_chk/01adsl_ip.csv' CSV HEADER DELIMITER ',';

