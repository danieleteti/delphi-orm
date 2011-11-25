DROP GENERATOR SEQ_PEOPLE_ID;
CREATE GENERATOR SEQ_PEOPLE_ID;
SET GENERATOR SEQ_PEOPLE_ID TO 0;
DROP TABLE PEOPLE;
CREATE TABLE PEOPLE(
  ID INTEGER not null primary key, FIRST_NAME VARCHAR(50), LAST_NAME VARCHAR(50), AGE INTEGER, BORN_DATE DATE, BORN_DATE_TIME TIMESTAMP, PHOTO BLOB SUB_TYPE 0 SEGMENT SIZE 16384);
DELETE FROM PEOPLE;
DROP GENERATOR SEQ_PHONES_ID;
CREATE GENERATOR SEQ_PHONES_ID;
SET GENERATOR SEQ_PHONES_ID TO 0;
DROP TABLE PHONES;
CREATE TABLE PHONES(
  ID INTEGER not null primary key, NUMBER VARCHAR(50), MODEL VARCHAR(50), ID_PERSON INTEGER);
DELETE FROM PHONES;
DROP GENERATOR SEQ_CARS_ID;
CREATE GENERATOR SEQ_CARS_ID;
SET GENERATOR SEQ_CARS_ID TO 0;
DROP TABLE CARS;
CREATE TABLE CARS(
  ID INTEGER not null primary key, BRAND VARCHAR(30), MODEL VARCHAR(30), ID_PERSON INTEGER);
DELETE FROM CARS;
DROP GENERATOR SEQ_EMAILS_ID;
CREATE GENERATOR SEQ_EMAILS_ID;
SET GENERATOR SEQ_EMAILS_ID TO 0;
DROP TABLE EMAILS;
CREATE TABLE EMAILS(
  ID INTEGER not null primary key, ADDRESS VARCHAR(100), ID_PERSON INTEGER);
DELETE FROM EMAILS;
