
/* Execute the code in following manner; */
/* iaim=> \i /cygdrive/d/Hospital_data/ProgresSQL/prgm/mrd2cnt.sql; */

drop table if exists mrd2cnt, mrd3cnt, mrd_diag, mrd2;
/*drop table if exists mrd3cnt;*/

create temp table mrd_diag as
select visit_id, description, icd_code, code_type, diag_type
from iaim.mrd_diagnosis;

create temp table pat_details as
select distinct mr_no, patient_id
from iaim.patient_section_details;

create temp table pat_demog as
select distinct mr_no, patient_gender,patient_city,patient_state,dateofbirth,country,oldmrno,remarks,death_date
from iaim.patient_details;

create temp table mrd2 as
select mrd_diag.*,
pat_details.*
from mrd_diag
left join pat_details
on mrd_diag.visit_id = pat_details.patient_id;

create temp table mrd2cnt as
select mr_no, count(distinct patient_id) from mrd2
group by mr_no;

create temp table mrd3cnt as
select mr_no, count(distinct icd_code) as ndis from mrd2
group by mr_no;

create temp table adsl01 as
select pat_demog.*, mrd3cnt.ndis
from pat_demog
full join mrd3cnt
on pat_demog.mr_no = mrd3cnt.mr_no;

\copy mrd2cnt TO 'd:/hospital_data/ProgresSQL/data_chk/mrd2cnt.csv' CSV HEADER DELIMITER ',';
\copy mrd3cnt TO 'd:/hospital_data/ProgresSQL/data_chk/mrd3cnt.csv' CSV HEADER DELIMITER ',';
\copy adsl01 TO 'd:/hospital_data/ProgresSQL/data_chk/adsl01.csv' CSV HEADER DELIMITER ',';
