       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-PROCESS.
      ******************************************************************
      * Program:     CUSTOMER-PROCESS
      * Author:      Development Team
      * Date:        2024-01-20
      * Purpose:     Process customer records and generate reports
      * 
      * Maintenance Log:
      * Date       Developer    Description
      * ---------- ------------ ----------------------------------------
      * 2024-01-20 DEV-TEAM    Initial version
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               FILE STATUS IS FILE-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO 'REPORT.DAT'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      * Customer Record Layout
      * CUSTOMER-ID:    9(6) - Unique identifier for each customer
      * CUST-NAME:      X(30) - Customer full name
      * CUST-ADDRESS:   X(50) - Customer street address
      * CUST-PHONE:     X(15) - Contact phone number
      * CUST-BALANCE:   9(7)V99 - Current account balance
      ******************************************************************
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID       PIC 9(6).
           05 CUST-NAME        PIC X(30).
           05 CUST-ADDRESS     PIC X(50).
           05 CUST-PHONE       PIC X(15).
           05 CUST-BALANCE     PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
           88 SUCCESS          VALUE "00".
           88 END-OF-FILE      VALUE "10".
           
       01 WS-COUNTERS.
           05 RECORDS-READ     PIC 9(6) VALUE ZERO.
           05 RECORDS-UPDATED  PIC 9(6) VALUE ZERO.

       PROCEDURE DIVISION.
      ******************************************************************
      * MAIN-PROCESS
      * Main processing routine that controls program flow
      * - Initializes the processing environment
      * - Processes all customer records
      * - Performs cleanup and creates final report
      ******************************************************************
       MAIN-PROCESS.
           PERFORM INITIALIZATION
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE
           PERFORM CLEANUP
           STOP RUN.

      ******************************************************************
      * INITIALIZATION
      * Opens required files and initializes working storage variables
      ******************************************************************
       INITIALIZATION.
           OPEN I-O CUSTOMER-FILE
           OPEN OUTPUT REPORT-FILE
           INITIALIZE WS-COUNTERS.

      ******************************************************************
      * PROCESS-RECORDS
      * Reads and processes each customer record
      * Updates balances and maintains counter information
      ******************************************************************
       PROCESS-RECORDS.
           READ CUSTOMER-FILE
               AT END SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO RECORDS-READ
                   PERFORM UPDATE-CUSTOMER-RECORD
           END-READ.

      ******************************************************************
      * UPDATE-CUSTOMER-RECORD
      * Updates individual customer record based on business rules
      * - Applies service charges if applicable
      * - Updates customer status
      ******************************************************************
       UPDATE-CUSTOMER-RECORD.
           IF CUST-BALANCE < ZERO
               ADD 25.00 TO CUST-BALANCE
               REWRITE CUSTOMER-RECORD
               ADD 1 TO RECORDS-UPDATED
           END-IF.

      ******************************************************************
      * CLEANUP
      * Performs end-of-job processing
      * - Closes files
      * - Generates processing summary
      ******************************************************************
       CLEANUP.
           CLOSE CUSTOMER-FILE
           CLOSE REPORT-FILE.
