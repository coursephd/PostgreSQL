Updates on 11th April 2018 afternoon India time:

/* 11th April 2018 */
/* SQL version with UNION of data */

/*=======================================================================*/
/* Execute the code in following manner;                                 */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/01adsl_union.sql; */
/*=======================================================================*/

drop table if exists 
temp0pat_demog, temp1pat_reg, temp1doc_cons, temp2reg_cons, temp2diag, temp3pat_presc, temp4pat_med,
temp20, temp30, temp30_5, temp100ip, temp350, temp100ser, temp100ser2, temp360,
base01_op0, base01_op, base01_ip, base01_ser, base_all ;

/* Create demog table */
create temp table temp0pat_demog as
select distinct mr_no as mrno, patient_gender, patient_city, patient_state, dateofbirth, country, /*oldmrno, remarks,*/ death_date
from iaim.patient_details;

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

create table temp30_5 as
select mr_no, patient_id, patient_gender, patient_city, patient_state, dateofbirth, country, death_date, 
consult_id, description, icd_code, diag_type, diagdate, patient_presc_id
from temp30;

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

create table base01_op as
select temp30_5.*, temp4pat_med.*
from temp4pat_med
left join temp30_5 on
temp30_5.patient_presc_id = temp4pat_med.op_medicine_pres_id
order by mr_no, patient_id;

/*create table base01_op as
select mr_no, patient_id, consultation_id, duration, duration_units, prescdate, frequency, quantity, remarks, cat_id, 
op_medicine_pres_id as med_form_id, doctor_id, presc_type
from base01_op0
order by mr_no, patient_id;
*/

/* IP medications */
create table temp100ip as
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

create table base01_ip as
select temp30_5.*, temp100ip.*
from temp100ip
left join temp30_5 on
temp30_5.patient_id = temp100ip.ippatient_id;

/*create table base01_ip as
select mr_no, patient_id, consultation_id, prescdate, quantity, med_route, remarks, cat_id, 
med_form_id, doctor_id, presc_type
from temp350
order by mr_no, patient_id; */

/* Services */

create table temp100ser as
select mr_no as ser_mrno, 
patient_id as ser_patient_id, 
service_id as cat_id, 
presc_date::timestamptz::date as prescdate, 
conducted, 
conductedby, 
conducteddate::timestamptz::date as sercond_date, 
prescription_id as consultation_id
from iaim.services_prescribed;

create table temp100ser2 as
select ser_mrno, ser_patient_id, cat_id, count(*) as frequency
from temp100ser
group by ser_mrno, ser_patient_id, cat_id;

/* Create Base01_ser */

create table base01_ser as
select temp30_5.*, temp100ser2.*
from temp100ser
left join temp30_5 on
temp30_5.mr_no = temp100ser2.ser_mrno and temp30_5.patient_id = temp100ser2.ser_patient_id;

/* UNION tables base01_op, base01_ip, base01_ser */

create table base_all as
select base01_op.*, Null as item_name, Null as doctor_id, Null as ippatient_id, Null as consultation_id from base01_op
union
select base01_ip.*, Null as op_medicine_pres_id, Null as duration, Null as duration_units from base01_ip
union
select base01_ser.*, Null as op_medicine_pres_id, Null as duration, Null as duration_units, Null as prescdate, Null as quantity, Null as remarks, Null as item_name, Null as doctor_id, Null as ippatient_id, Null as consultation_id from base01_ser;

/*======================================================================================================================================*/


/* 11th April 2018 */
/* SQL version with UNION of data */

/*=======================================================================*/
/* Execute the code in following manner;                                 */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/01adsl_union.sql; */
/*=======================================================================*/

drop table if exists 
temp0pat_demog, temp1pat_reg, temp1doc_cons, temp2reg_cons, temp2diag, temp3pat_presc, temp4pat_med,
temp20, temp30, temp100ip, temp350, temp100ser, temp100ser2, temp360,
base01_op0, base01_op, base01_ip, base01_ser, base_all ;

/* Create demog table */
create temp table temp0pat_demog as
select distinct mr_no as mrno, patient_gender, patient_city, patient_state, dateofbirth, country, /*oldmrno, remarks,*/ death_date
from iaim.patient_details;

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

create table base01_op0 as
select temp30.*, temp4pat_med.*
from temp4pat_med
left join temp30 on
temp30.patient_presc_id = temp4pat_med.op_medicine_pres_id;

create table base01_op as
select mr_no, patient_id, consultation_id, duration, duration_units, prescdate, frequency, quantity, remarks, cat_id, 
op_medicine_pres_id as med_form_id, doctor_id, presc_type
from base01_op0
order by mr_no, patient_id;


/* IP medications */

create table temp100ip as
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

create table temp350 as
select temp30.mr_no, temp30.patient_id, temp100ip.*
from temp100ip
left join temp30 on
temp30.patient_id = temp100ip.ippatient_id;

create table base01_ip as
select mr_no, patient_id, consultation_id, prescdate, quantity, med_route, remarks, cat_id, 
med_form_id, doctor_id, presc_type
from temp350
order by mr_no, patient_id;

/* Services */

create table temp100ser as
select mr_no, 
patient_id, 
service_id, 
doctor_id, 
presc_date::timestamptz::date as prescdate, 
conducted, conductedby, 
conducteddate::timestamptz::date as sercond_date, 
prescription_id as consultation_id
from iaim.services_prescribed;

/* Create Base01_ser */

create table base01_ser as
select mr_no, patient_id, service_id, count(*) as nofservice
from temp100ser
group by mr_no, patient_id, service_id;


/* UNION tables base01_op, base01_ip, base01_ser */

create table base_all as
select base01_op.* from base01_op
union
select base01_ip.* from base01_ip
union
select base01_ser.* from base01_ser;

