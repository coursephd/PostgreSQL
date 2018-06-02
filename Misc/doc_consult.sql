/* Execute the code in following manner; */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/doc_consult.sql; */

drop table if exists doc_consult, doc_date;

create temp table doc_consult as
select mr_no, count(distinct patient_id) as nvis
from iaim.doctor_consultation
group by mr_no;

create temp table doc_date as
select mr_no, date (visited_date ) as dateonly, consultation_id
from iaim.doctor_consultation
order by mr_no, dateonly;

/* patient_prescription = consultation_id */
/* A Subset is required */
create temp table pat_presc as
select consultation_id, presc_type, status
from iaim.patient_prescription
order by consultation_id;


/* patient_medicine_prescriptions = medicine_id */
create temp table pat_med as
select medicine_id, op_medicine_pres_id, duration, duration_units, date(mod_time) as dateonly, frequency, medicine_quantity, medicine_remarks
from iaim.patient_medicine_prescriptions
order by medicine_id, op_medicine_pres_id;


/* date_part(visited_date, timestamp) as visdate */

\copy doc_consult TO 'd:/hospital_data/ProgresSQL/data_chk/doc_consult.csv' CSV HEADER DELIMITER ',';
\copy doc_date TO 'd:/hospital_data/ProgresSQL/data_chk/doc_date.csv' CSV HEADER DELIMITER ',';
\copy pat_presc TO 'd:/hospital_data/ProgresSQL/data_chk/pat_presc.csv' CSV HEADER DELIMITER ',';
\copy pat_med TO 'd:/hospital_data/ProgresSQL/data_chk/pat_med.csv' CSV HEADER DELIMITER ',';

/*
iaim=> \d+ iaim.patient_prescription
 patient_presc_id         | integer                     |           | not null |                                               | plain    |              |
 mr_no                    | character varying(15)       |           |          |                                               | extended |              |
 consultation_id          | integer                     |           |          |                                               | plain    |              |
 presc_type               | character varying(50)       |           |          |                                               | extended |              |
 store_item               | boolean                     |           |          | false                                         | plain    |              |
 status                   | character varying(5)        |           |          |                                               | extended |              |
 no_order_reason          | character varying(1000)     |           |          |                                               | extended |              |
 prescribed_date          | timestamp without time zone |           |          | ('now'::text)::timestamp(0) without time zone | plain    |              |
 obsolete_prescription_id | integer                     |           |          |                                               | plain    |              |
 conducting_personnel     | character varying(30)       |           |          |                                               | extended |              |
 visit_id                 | character varying(15)       |           |          |                                               | extended |              |
 cancelled_datetime       | timestamp without time zone |           |          |                                               | plain    |              |
 cancelled_by             | character varying(30)       |           |          |                                               | extended |              |
 pri_pre_auth_no          | character varying(15)       |           |          |                                               | extended |              |
 pri_pre_auth_mode_id     | integer                     |           |          |                                               | plain    |              |
 sec_pre_auth_no          | character varying(15)       |           |          |                                               | extended |              |
 sec_pre_auth_mode_id     | integer                     |           |          |                                               | plain    |              |
 special_instr            | character varying(2000)     |           |          |                                               | extended |              |
 external_order_no        | character varying           |           |          |                                               | extended |              |

513177
*/
