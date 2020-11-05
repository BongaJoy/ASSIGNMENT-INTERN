      ******************************************************************
      * Author: JOYCE MACHABA
      * Date: 11/04/2020
      * Purpose: CREATE AN INDEX FILE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                  DIVISION.

       PROGRAM-ID.                     CREATE-IDX-FIE.
       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
           SELECT  DATA-FILE           ASSIGN TO "DATA-FILE.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT  SURVEY-IDX          ASSIGN TO "SURVEY-IDX.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS WS-ID.


       DATA                            DIVISION.

       FILE                            SECTION.
       FD DATA-FILE.
       01 F-DATA-REC.
           05 F-PERSONAL-DETAILS.
             10 F-WS-ID                  PIC 999.
             10 F-WS-SURNAME             PIC X(20).
             10 F-WS-FIRST-NAMES         PIC X(30).
             10 F-WS-CONTACT-NUMBER      PIC 9(10).
             10 F-WS-DATE                PIC 9(08).
             10 F-WS-AGE                 PIC 9(02).
           05  F-WS-FAVOURITE-FOOD.
             10 F-WS-FAVOUR-PIZZA        PIC X.
             10 F-WS-FAVOUR-PASTA        PIC X.
             10 F-WS-FAVOUR-PAP          PIC X.
             10 F-WS-FAVOUR-CHICKEN      PIC X.
             10 F-WS-FAVOUR-BEEF         PIC X.
             10 F-WS-OTHER-FAV           PIC X.
           05 F-SCALES.
             10 F-OUT-SCALE.
               15 F-OUT-SCAL1            PIC X.
               15 F-OUT-SCAL2            PIC X.
               15 F-OUT-SCAL3            PIC X.
               15 F-OUT-SCAL4            PIC X.
               15 F-OUT-SCAL5            PIC X.
             10 F-MOVIE-SCALE.
               15 F-MOVIE-SCAL1          PIC X.
               15 F-MOVIE-SCAL2          PIC X.
               15 F-MOVIE-SCAL3          PIC X.
               15 F-MOVIE-SCAL4          PIC X.
               15 F-MOVIE-SCAL5          PIC X.
             10 F-TV-SCALE.
               15 F-TV-SCAL1             PIC X.
               15 F-TV-SCAL2             PIC X.
               15 F-TV-SCAL3             PIC X.
               15 F-TV-SCAL4             PIC X.
               15 F-TV-SCAL5             PIC X.
             10 F-RADIO-SCALE.
               15 F-RADIO-SCAL1          PIC X.
               15 F-RADIO-SCAL2          PIC X.
               15 F-RADIO-SCAL3          PIC X.
               15 F-RADIO-SCAL4          PIC X.
               15 F-RADIO-SCAL5          PIC X.

       FD SURVEY-IDX.
       01  DATA-REC.
           05 PERSONAL-DETAILS.
             10 WS-ID                  PIC 999.
             10 WS-NAME                PIC X(20).
             10 WS-FIRST-NAMES         PIC X(30).
             10 WS-CONTACT-NUMBER      PIC 9(10).
             10 WS-DATE                PIC 9(08).
             10 WS-AGE                 PIC 9(02).
           05  WS-FAVOURITE-FOOD.
             10 WS-FAVOUR-PIZZA        PIC X.
             10 WS-FAVOUR-PASTA        PIC X.
             10 WS-FAVOUR-PAP          PIC X.
             10 WS-FAVOUR-CHICKEN      PIC X.
             10 WS-FAVOUR-BEEF         PIC X.
             10 WS-OTHER-FAV           PIC X.
           05 SCALES.
             10 OUT-SCALE.
               15 OUT-SCAL1            PIC X.
               15 OUT-SCAL2            PIC X.
               15 OUT-SCAL3            PIC X.
               15 OUT-SCAL4            PIC X.
               15 OUT-SCAL5            PIC X.
             10 MOVIE-SCALE.
               15 MOVIE-SCAL1          PIC X.
               15 MOVIE-SCAL2          PIC X.
               15 MOVIE-SCAL3          PIC X.
               15 MOVIE-SCAL4          PIC X.
               15 MOVIE-SCAL5          PIC X.
             10 TV-SCALE.
               15 TV-SCAL1             PIC X.
               15 TV-SCAL2             PIC X.
               15 TV-SCAL3             PIC X.
               15 TV-SCAL4             PIC X.
               15 TV-SCAL5             PIC X.
             10 RADIO-SCALE.
               15 RADIO-SCAL1          PIC X.
               15 RADIO-SCAL2          PIC X.
               15 RADIO-SCAL3          PIC X.
               15 RADIO-SCAL4          PIC X.
               15 RADIO-SCAL5          PIC X.

       WORKING-STORAGE                 SECTION.
           01 WG-EOF                   PIC XX.
       PROCEDURE                       DIVISION.
       AA000-MAIN-PROCEDURE            SECTION.
           PERFORM BA000-INITIAL

           PERFORM UNTIL WG-EOF        = HIGH-VALUES
              DISPLAY F-DATA-REC
              MOVE F-DATA-REC          TO DATA-REC
              WRITE DATA-REC
              PERFORM ZA000-READ-DATA-FILE
           END-PERFORM.

           PERFORM ZZ000-TERMINATE.
           STOP RUN.

       BA000-INITIAL                   SECTION.
           OPEN INPUT DATA-FILE.
           OPEN OUTPUT SURVEY-IDX.

           MOVE LOW-VALUES             TO WG-EOF.
           PERFORM ZA000-READ-DATA-FILE.

       ZA000-READ-DATA-FILE            SECTION.
           READ DATA-FILE
               AT END
                   MOVE HIGH-VALUES    TO WG-EOF.

       ZZ000-TERMINATE                 SECTION.
           CLOSE DATA-FILE SURVEY-IDX.
