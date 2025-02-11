--Script cannot be run on OSCER, must be run on SQL
--Create table

CREATE TABLE insurance_data (
    policyID INT,
    statecode TEXT,
    county TEXT,
    eq_site_limit FLOAT,
    hu_site_limit FLOAT,
    fl_site_limit FLOAT,
    fr_site_limit FLOAT,
    tiv_2011 FLOAT,
    tiv_2012 FLOAT,
    eq_site_deductible FLOAT,
    hu_site_deductible FLOAT,
    fl_site_deductible FLOAT,
    fr_site_deductible FLOAT,
    point_latitude FLOAT,
    point_longitude FLOAT,
    line TEXT,
    construction TEXT,
    point_granularity INT
);

--Read in the FL_insurance_sample.csv file 

.mode csv
import FL_insurance_sample.csv insurance_data

--Display the first 10 lines
SELECT * FROM insurance_data LIMIT 10; 

--Print the unique counties in the data
SELECT DISTINCT county FROM insurance_data;

--Calculate the average appreciation rate
SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM insurance_data;

--Create a frequency table of the construction variable
SELECT construction, COUNT(*) FROM insurance_data 
GROUP BY construction ORDER BY COUNT(*) DESC;
