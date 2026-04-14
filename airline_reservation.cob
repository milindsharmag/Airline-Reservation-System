       IDENTIFICATION DIVISION.
       PROGRAM-ID. AIRLINE-RESERVATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FLIGHT-FILE ASSIGN TO 'FLIGHT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FLIGHT-ID.

           SELECT PASS-FILE ASSIGN TO 'PASS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PASS-ID.

       DATA DIVISION.
       FILE SECTION.

       FD FLIGHT-FILE.
       01 FLIGHT-REC.
           05 FLIGHT-ID        PIC X(5).
           05 FLIGHT-NAME      PIC X(20).
           05 AVAILABLE-SEATS  PIC 9(3).

       FD PASS-FILE.
       01 PASS-REC.
           05 PASS-ID          PIC X(5).
           05 PASS-NAME        PIC X(20).
           05 PASS-FLIGHT-ID   PIC X(5).
           05 STATUS           PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-CHOICE        PIC 9.
       01 WS-FLIGHT-ID     PIC X(5).
       01 WS-PASS-ID       PIC X(5).
       01 WS-PASS-NAME     PIC X(20).

       PROCEDURE DIVISION.

       MAIN-PARA.
           DISPLAY "==== AIRLINE RESERVATION SYSTEM ====".
           DISPLAY "1. BOOK TICKET".
           DISPLAY "2. CANCEL TICKET".
           DISPLAY "3. DISPLAY FLIGHT".
           DISPLAY "ENTER CHOICE: ".
           ACCEPT WS-CHOICE.

           EVALUATE WS-CHOICE
               WHEN 1 PERFORM BOOK-TICKET
               WHEN 2 PERFORM CANCEL-TICKET
               WHEN 3 PERFORM DISPLAY-FLIGHT
               WHEN OTHER DISPLAY "INVALID CHOICE"
           END-EVALUATE.

           STOP RUN.

       *---------------- BOOK ----------------*
       BOOK-TICKET.
           OPEN I-O FLIGHT-FILE PASS-FILE.

           DISPLAY "ENTER FLIGHT ID: ".
           ACCEPT WS-FLIGHT-ID.

           MOVE WS-FLIGHT-ID TO FLIGHT-ID.
           READ FLIGHT-FILE INVALID KEY
               DISPLAY "FLIGHT NOT FOUND"
               GO TO END-BOOK.

           IF AVAILABLE-SEATS > 0
               DISPLAY "ENTER PASSENGER ID: ".
               ACCEPT WS-PASS-ID.

               DISPLAY "ENTER PASSENGER NAME: ".
               ACCEPT WS-PASS-NAME.

               SUBTRACT 1 FROM AVAILABLE-SEATS
               REWRITE FLIGHT-REC

               MOVE WS-PASS-ID TO PASS-ID
               MOVE WS-PASS-NAME TO PASS-NAME
               MOVE WS-FLIGHT-ID TO PASS-FLIGHT-ID
               MOVE "BOOKED" TO STATUS

               WRITE PASS-REC INVALID KEY
                   DISPLAY "PASSENGER ALREADY EXISTS"

               DISPLAY "BOOKING SUCCESSFUL"
           ELSE
               DISPLAY "NO SEATS AVAILABLE"
           END-IF.

       END-BOOK.
           CLOSE FLIGHT-FILE PASS-FILE.

       *---------------- CANCEL ----------------*
       CANCEL-TICKET.
           OPEN I-O FLIGHT-FILE PASS-FILE.

           DISPLAY "ENTER PASSENGER ID: ".
           ACCEPT WS-PASS-ID.

           MOVE WS-PASS-ID TO PASS-ID.
           READ PASS-FILE INVALID KEY
               DISPLAY "PASSENGER NOT FOUND"
               GO TO END-CANCEL.

           MOVE PASS-FLIGHT-ID TO FLIGHT-ID.
           READ FLIGHT-FILE INVALID KEY
               DISPLAY "FLIGHT NOT FOUND"
               GO TO END-CANCEL.

           ADD 1 TO AVAILABLE-SEATS
           REWRITE FLIGHT-REC

           MOVE "CANCELLED" TO STATUS
           REWRITE PASS-REC

           DISPLAY "TICKET CANCELLED SUCCESSFULLY".

       END-CANCEL.
           CLOSE FLIGHT-FILE PASS-FILE.

       *---------------- DISPLAY ----------------*
       DISPLAY-FLIGHT.
           OPEN INPUT FLIGHT-FILE.

           DISPLAY "ENTER FLIGHT ID: ".
           ACCEPT WS-FLIGHT-ID.

           MOVE WS-FLIGHT-ID TO FLIGHT-ID.
           READ FLIGHT-FILE INVALID KEY
               DISPLAY "FLIGHT NOT FOUND"
               GO TO END-DISP.

           DISPLAY "FLIGHT NAME: " FLIGHT-NAME.
           DISPLAY "AVAILABLE SEATS: " AVAILABLE-SEATS.

       END-DISP.
           CLOSE FLIGHT-FILE.

       END PROGRAM AIRLINE-RESERVATION.
