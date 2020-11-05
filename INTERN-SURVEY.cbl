      ******************************************************************
      * Author: JOYCE
      * Date: 11/02/2020
      * Purpose: IS TO CREATE A SMALL SYSTEM THAT WILLL USERS TO TAKE A SURVEY
      *          AND STORE THE DATA ON A FILE(DATABASE) AND ALSO RETRIEVE
      *          THE DATA BACK TO DO RELEVENT CALCULATIONS.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION                  DIVISION.

       PROGRAM-ID.                     INTERN-SURVEY.
       ENVIRONMENT                     DIVISION.

       CONFIGURATION                   SECTION.

       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *THE NAME OF THE FILE THAT AM STORING THE DATA AND THE PRIMARY KEY WILL THE USER ID.
           SELECT  SURVEY-DATA         ASSIGN TO "SURVEY-DATA.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS WS-ID.


       DATA                            DIVISION.

       FILE                            SECTION.
      *FD STRUCTURE FOR THE FILE OF DATA....
       FD SURVEY-DATA.
       01  DATA-REC.
           05 PERSONAL-DETAILS.
             10 WS-ID                  PIC 9(13).
             10 WS-NAME                PIC X(20).
             10 WS-FIRST-NAMES         PIC X(30).
             10 WS-CONTACT-NUMBER      PIC 9(10).
             10 WS-DATE.
                15 WS-MM               PIC 99.
                15 WS-DD               PIC 99.
                15 WS-YEAR             PIC 9999.
             10 WS-AGE                 PIC 99.
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
      *VARIALES I HAVE USED FOR THE PROGRAM......
       01  RESPONSEZ.
           05  RESPONSE-IN-WS          PIC X.

       01 WS-OK                        PIC X.
       01 WS-OPTION-RESPOND            PIC 9.
       01 PIZZA-COUNT                  PIC 99.
       01 PASTA-COUNT                  PIC 99.
       01 PAP-N-WORS-COUNT             PIC 99.
       01 OUT-COUNT                    PIC 99.
       01 MOVIES-COUNT                 PIC 99.
       01 TV-COUNT                     PIC 99.
       01 RADIO-COUNT                  PIC 99.
       01 WS-EOF                       PIC XX.
       01 ERR-MESSAGE                  PIC X(100).
       01 WS-TOTAL-AGE                 PIC 999.

       01 WS-SURVEY-COUNT              PIC 9999.
       01 WS-AVAG-AGE                  PIC 999.
       01 WS-MAX-AGE                   PIC 999.
       01 WS-MIN-AGE                   PIC 999.

       01 WS-PERC-PIZZA                PIC 999V9.
       01 WS-PERC-PASTA                PIC 999V9.
       01 WS-PERC-PAP                  PIC 999V9.

       01 WS-OUT-AVAG                  PIC 999V9.
       01 WS-MOVIE-AVAG                PIC 999V9.
       01 WS-TV-AVAG                   PIC 999V9.
       01 WS-RADIO-AVAG                PIC 999V9.

       01 WS-TEMP-AGE                  PIC 99.
       SCREEN SECTION.
       01 MAIN-SCREEN.
      *THE MAIN SCREEN......
           05  VALUE "WELCOME " BLANK SCREEN             LINE 2 COL 50.
            05  VALUE "----------------------"           LINE 3 COL 42.
           05  VALUE "1) Fill out survey"                LINE 5 COL 42.
           05  VALUE "2) View survey results"            LINE 6 COL 42.
           05  OPTION-INPUT                              LINE 8 COL 50
                                       PIC  9      TO WS-OPTION-RESPOND.
      *SECOND SCREEN......
       01  DATA-ENTRY-SCREEN.
           05  VALUE "TAKE OUR SURVEY " BLANK SCREEN     LINE 1 COL 2.
           05  VALUE "Personal Details  :"               LINE 3 COL 2.
           05  VALUE "* ID NUMBER       :"               LINE 5 COL 10.
           05  ID-INPUT                                  LINE 5 COL 35
                                       PIC  9(13)  TO WS-ID.

           05  VALUE "Surname           :"               LINE 7 COL 10.
           05  SURNAME-INPUT                             LINE 7 COL 35
                                       PIC  X(30)  TO WS-NAME.

           05  VALUE "First Names       :"               LINE 9 COL 10.
           05  NAMES-INPUT                               LINE 9 COL 35
                                       PIC X(30)   TO WS-FIRST-NAMES.

           05  VALUE "Contact Number    :"               LINE 11 COL 10.
           05  CONTACT-INPUT                             LINE 11 COL 35
                                       PIC 9(10)   TO WS-CONTACT-NUMBER.


           05  VALUE "Date(MM/DD/YYYY)  :"               LINE 13 COL 10.
           05  MM-INPUT                                  LINE 13 COL 35
                                       PIC 9(02)   TO WS-MM.
           05  VALUE "/"                                 LINE 13 COL 37.
           05  DD-INPUT                                  LINE 13 COL 38
                                       PIC 9(02)   TO WS-DD.
           05  VALUE "/"                                 LINE 13 COL 40.
           05  YEAR-INPUT                                LINE 13 COL 41
                                       PIC 9(04)   TO WS-YEAR.

           05  VALUE "Age               :"               LINE 15 COL 10.
           05  NAMES-INPUT                               LINE 15 COL 35
                                       PIC 9(02)   TO WS-AGE.

           05  VALUE
           "What is your favourite food? (YOU CAN CHOOSE MORE THAN 1)"
                                                         LINE 17 COL 2.
           05  VALUE
           "NOTE: ****** CHOOSE BY USING AN X"           LINE 17 COL 65.
           05  VALUE "Pizza"                             LINE 18 COL 10.
           05  CHOICE1-INPUT                             LINE 18 COL 7
                                       PIC  X      TO WS-FAVOUR-PIZZA.

           05  VALUE "Pasta"                             LINE 19 COL 10.
           05  CHOICE2-INPUT                             LINE 19 COL 7
                                       PIC  X      TO WS-FAVOUR-PASTA.

           05  VALUE "Pap and wors"                      LINE 20 COL 10.
           05  CHOICE3-INPUT                             LINE 20 COL 7
                                       PIC  X      TO WS-FAVOUR-PAP.

           05  VALUE "Chicken stir fry"                  LINE 21 COL 10.
           05  CHOICE4-INPUT                             LINE 21 COL 7
                                       PIC  X      TO WS-FAVOUR-CHICKEN.

           05  VALUE "Beef stir fry"                     LINE 22 COL 10.
           05  CHOICE5-INPUT                             LINE 22 COL 7
                                       PIC  X      TO WS-FAVOUR-BEEF.

           05  VALUE "Other"                             LINE 23 COL 10.
           05  CHOICE6-INPUT                             LINE 23 COL 7
                                       PIC  X      TO WS-OTHER-FAV.

           05  VALUE "ENTER (Y) TO CONTINUE >>>>"        LINE 25 COL 45.
           05  RESPONSE-INPUT                            LINE 25 COL 74
                                       PIC X       TO RESPONSE-IN-WS.
           05 ERR-INPUT                                  LINE 27 COL 35
                                       PIC X(100)   FROM ERR-MESSAGE.


      *SECOND SCREEN CONTINUED....
       01  DATA-ENTRY2-SCREEN.
           05  VALUE "CONTINUE......" BLANK SCREEN       LINE 1 COL 2.
           05  VALUE
           "scale FROM 1 to 5 indicate whether you agree or disagree"
     -                                                   LINE 2 COL 2.
           05  VALUE "****FILL IN WITH X "               LINE 3 COL 2.
           05  VALUE "STRONGLY AGREE"                    LINE 4 COL 35.
           05  VALUE "AGREE"                             LINE 4 COL 55.
           05  VALUE "NEUTRAL"                           LINE 4 COL 65.
           05  VALUE "DISAGREE"                          LINE 4 COL 75.
           05  VALUE "STRONGY DISAGREE"                  LINE 4 COL 85.

           05 VALUE "I like to eat out"                  LINE 5 COL 5.
           05 STR-AGREE1                                 LINE 5 COL 40
                                      PIC X        TO OUT-SCAL1.
           05 AGREE1                                     LINE 5 COL 57
                                      PIC X        TO OUT-SCAL2.
           05 NEUTRAL1                                   LINE 5 COL 67
                                      PIC X        TO OUT-SCAL3.
           05 DISAGREE1                                  LINE 5 COL 78
                                      PIC X        TO OUT-SCAL4.
           05 STR-DISAGREE1                              LINE 5 COL 90
                                      PIC X        TO OUT-SCAL5.

           05 VALUE "I like to watch movies"             LINE 7 COL 5.
           05 STR-AGREE2                                 LINE 7 COL 40
                                      PIC X        TO MOVIE-SCAL1.
           05 AGREE2                                     LINE 7 COL 57
                                      PIC X        TO MOVIE-SCAL2.
           05 NEUTRAL2                                   LINE 7 COL 67
                                      PIC X        TO MOVIE-SCAL3.
           05 DISAGREE2                                  LINE 7 COL 78
                                      PIC X        TO MOVIE-SCAL4.
           05 STR-DISAGREE2                              LINE 7 COL 90
                                      PIC X        TO MOVIE-SCAL5.

           05 VALUE "I like to watch TV"                  LINE 9 COL 5.
           05 STR-AGREE3                                  LINE 9 COL 40
                                      PIC X        TO TV-SCAL1.
           05 AGREE3                                      LINE 9 COL 57
                                      PIC X        TO TV-SCAL2.
           05 NEUTRAL3                                    LINE 9 COL 67
                                      PIC X        TO TV-SCAL3.
           05 DISAGREE3                                   LINE 9 COL 78
                                      PIC X        TO TV-SCAL4.
           05 STR-DISAGREE3                               LINE 9 COL 90
                                      PIC X        TO TV-SCAL5.

           05 VALUE "I like to listen to the radio"       LINE 11 COL 5.
           05 STR-AGREE4                                  LINE 11 COL 40
                                      PIC X        TO RADIO-SCAL1.
           05 AGREE4                                      LINE 11 COL 57
                                      PIC X        TO RADIO-SCAL2.
           05 NEUTRAL4                                    LINE 11 COL 67
                                      PIC X        TO RADIO-SCAL3.
           05 DISAGREE4                                   LINE 11 COL 78
                                      PIC X        TO RADIO-SCAL4.
           05 STR-DISAGREE4                               LINE 11 COL 90
                                      PIC X        TO RADIO-SCAL5.

           05  VALUE "ENTER (Y) TO SUBMIT >>>>"          LINE 24 COL 45.
           05  RESPONSE-INPUT                            LINE 24 COL 74
                                       PIC X       TO RESPONSE-IN-WS.


      *THIRD SCREEN......
       01 RESULTS-SCREEN.
           05  VALUE "RESULTS OF THE SURVEY" BLANK SCREEN LINE 2 COL 40.
           05  VALUE "Total number of surveys:"           LINE 4 COL 10.
           05  SURVEY-INPUT                               LINE 4 COL 60
                                       PIC  9999   FROM WS-SURVEY-COUNT.

           05  VALUE "Average age:"                       LINE 5 COL 10.
           05  AVEGAGE-INPUT                              LINE 5 COL 60
                                       PIC  999    FROM WS-AVAG-AGE.

           05  VALUE "Oldest person who participated in survey:"
                                                          LINE 6 COL 10.
           05  oldest-INPUT                               LINE 6 COL 60
                                       PIC  999    FROM WS-MAX-AGE.

           05  VALUE "Youngest person who participated in survey:"
                                                          LINE 7 COL 10.
           05  youngst-INPUT                              LINE 7 COL 60
                                       PIC  999    FROM WS-MIN-AGE.

           05  VALUE "Percentage of people who like pizza:"
                                                          LINE 9 COL 10.
           05  PIZZA-INPUT                                LINE 9 COL 60
                                       PIC  999.9    FROM WS-PERC-PIZZA.
           05  VALUE "Percentage of people who like pasta:"
                                                         LINE 10 COL 10.
           05  PASTA-INPUT                               LINE 10 COL 60
                                       PIC  999.9    FROM WS-PERC-PASTA.
           05  VALUE "Percentage of people who like pap and wors:"
                                                         LINE 11 COL 10.
           05  PAP-INPUT                                 LINE 11 COL 60
                                       PIC  999.9    FROM WS-PERC-PAP.

           05  VALUE "People like to eat out:"
                                                         LINE 13 COL 10.
           05  out-INPUT                                 LINE 13 COL 60
                                       PIC  999.9    FROM WS-OUT-AVAG.

           05  VALUE "People like to watch movies:"
                                                         LINE 14 COL 10.
           05  movies-INPUT                              LINE 14 COL 60
                                       PIC  999.9    FROM WS-MOVIE-AVAG.

           05  VALUE "People like to watch TV:"
                                                         LINE 15 COL 10.
           05  TV-INPUT                                  LINE 15 COL 60
                                       PIC  999.9    FROM WS-TV-AVAG.

           05  VALUE "People like to listen to the radio:"
                                                         LINE 16 COL 10.
           05  RADIO-INPUT                               LINE 16 COL 60
                                       PIC  999.9    FROM WS-RADIO-AVAG.



           05  VALUE "OK >>>>"                           LINE 24 COL 45.
           05  RESPONSE-INPUT                            LINE 24 COL 74
                                       PIC X       TO WS-OK.



       PROCEDURE                       DIVISION.

       AA000-MAIN-PROCEDURE            SECTION.
      *OPENING THE FILE WHERE AM GOING TO STORE THE SURVEY DATA AND ALSO TO BE ABLE TO READ THE DATA...
           OPEN I-O SURVEY-DATA.

      *DISPLAYING THE MAIN SCREEN TO CHOOSE WHETHER YOU TAKE A SURVEY OR CHECK SURVEYS RESULTS...
           DISPLAY MAIN-SCREEN
           ACCEPT MAIN-SCREEN

      *EVALUATING THE OPTION YOU HAVE CHOSEN AND THEN TAKE YOU TO THE RELEVENT SCREEN...
      *WHEN YOU CHOOSE OPTION 1 IT WILL TAKE YOU TO THE TAKE SURVEY SECTION...
      *WHEN YOU CHOOSE OPTION 2 IT WILL TAKE YOU TO THE RESULTS OF THE SURVEY SECTION
           EVALUATE WS-OPTION-RESPOND
             WHEN 1
               PERFORM BA000-TAKE-A-SURVEY
             WHEN 2
               PERFORM CA000-VIEW-SURVEY-RESULTS
           END-EVALUATE.

      *CLOSING THE FILE AT THE END OF THE PROGRAM......
           CLOSE SURVEY-DATA.
           STOP RUN.

       BA000-TAKE-A-SURVEY             SECTION.
      *DISPLAYING THE TAKE A SURVEY SCREEN WHERE YOU WILL INPUT YOUR DATA......
           DISPLAY DATA-ENTRY-SCREEN.
           ACCEPT DATA-ENTRY-SCREEN.

      *WHEN YOU ENTER "Y" TO CONTINUE YOU WILL CONTINUE WITH THE SURVEY.....
      *IF YOU PUT SOMETHING ELSE IT WILL GIVE YOU AN ERROR MESSAGE....
             IF RESPONSE-IN-WS         = "Y"
                 PERFORM GA000-VALIDATIONS
                 IF ERR-MESSAGE NOT = SPACES
                    DISPLAY DATA-ENTRY-SCREEN
                    ACCEPT DATA-ENTRY-SCREEN
                  ELSE
                   DISPLAY DATA-ENTRY2-SCREEN
                   ACCEPT DATA-ENTRY2-SCREEN
                 END-IF
              ELSE
                 MOVE "ENTER (Y) TO CONTINUE"     TO ERR-MESSAGE
                 DISPLAY DATA-ENTRY-SCREEN
                 ACCEPT DATA-ENTRY-SCREEN

           END-IF.

      *AFTER FILLING IN YOUR DETAILS IT WILL WRITE YOUR DATA AND YOUR RATINGS TO THE FILE....
           WRITE DATA-REC.


       CA000-VIEW-SURVEY-RESULTS       SECTION.
      *WHEN YOU CHOOSED TO VIEW RESULTS OF SURVEY IT WILL FIRST CALCULATE THE TOTALS THEN DISPLAY THE RESULTS TO YOU
      *THE PERFORM STATEMENT WILL CALL THE CALCULATIONS SECTION...
           PERFORM DA000-CALCULATE-TOTAL.
           DISPLAY RESULTS-SCREEN.
           ACCEPT RESULTS-SCREEN.

      *WHEN YOU ENTER "Y" FOR OK IT WILL TAKE YOU BACK TO THE MAIN SCREEN.....
             IF WS-OK                  = "Y"
                DISPLAY MAIN-SCREEN
                ACCEPT MAIN-SCREEN
             END-IF.

       DA000-CALCULATE-TOTAL           SECTION.
      *THIS SECTION WILL FIRST READ FROM THE FILE TO DO THE CALCULATIONS......
           MOVE LOW-VALUES             TO WS-EOF.
           PERFORM ZA000-READ-FILE.

      * IT WILL DO THE PERFORM STATEMENT UNTIL END OF FILE WILL DOING THE CALCULATIONS...
           PERFORM UNTIL WS-EOF        = HIGH-VALUES
              ADD 01                   TO WS-SURVEY-COUNT
              COMPUTE WS-TOTAL-AGE     = WS-AGE + WS-TOTAL-AGE
              COMPUTE WS-AVAG-AGE      = WS-TOTAL-AGE / WS-SURVEY-COUNT
              PERFORM EA000-HIGHEST-AND-LOWEST
              PERFORM ZA000-READ-FILE
           END-PERFORM.

           CLOSE SURVEY-DATA.


       EA000-HIGHEST-AND-LOWEST        SECTION.
      *CHECKING FOR THE HIGHEST AND LOWEST AGE .........
           IF WS-MAX-AGE = 0           AND WS-MIN-AGE = 0
               MOVE WS-AGE             TO WS-MAX-AGE
               MOVE WS-AGE             TO WS-MIN-AGE
             ELSE
                IF WS-AGE              > WS-MAX-AGE
                    MOVE WS-AGE        TO WS-MAX-AGE
                 ELSE
                    IF WS-AGE          < WS-MIN-AGE
                       MOVE WS-AGE     TO WS-MIN-AGE
                    END-IF
                END-IF
           END-IF.

      *CALL SECTION TO CALCULATE THE PERCENTAGE OF FOOD PEOPLE LIKE.....
           PERFORM FA000-FOOD-PERCENTAGE.

       FA000-FOOD-PERCENTAGE           SECTION.
      *CHECKING WHICH FOOD THE LOVE AND ADD TOTALS ACCORDING TO GET THE PERCENTAGE....
           IF WS-FAVOUR-PIZZA          = "X"
               ADD 01                  TO PIZZA-COUNT
            ELSE
               IF WS-FAVOUR-PASTA      = "X"
                   ADD 01              TO PASTA-COUNT
                ELSE
                   IF WS-FAVOUR-PAP    = "X"
                      ADD 01           TO PAP-N-WORS-COUNT
                    END-IF
               END-IF
           END-IF.

      *CALCULATING THE PERCENTAGE OF PEOPLE WHO LIKE PIZZA, PASTA, AND PAP&WORS.....
           COMPUTE WS-PERC-PIZZA = PIZZA-COUNT/WS-SURVEY-COUNT * 100.
           COMPUTE WS-PERC-PASTA = PASTA-COUNT/WS-SURVEY-COUNT * 100.
           COMPUTE WS-PERC-PAP = PAP-N-WORS-COUNT/WS-SURVEY-COUNT * 100.
           PERFORM HA000-AVERAGE0FRATING.

       HA000-AVERAGE0FRATING                  SECTION.
      *CHECK PEOPLE WHO AGREED AND STRONGLY AGREE TO GET THE TOTAL NUMBER OF PEOPLE ACCORDING TO WHAT THEY LIKE..
           IF OUT-SCAL1 = "X"           OR OUT-SCAL2 = "X"
               ADD 01                   TO OUT-COUNT
            ELSE
               IF MOVIE-SCAL1 = "X"     OR MOVIE-SCAL2 = "X"
                   ADD 01               TO MOVIES-COUNT
                ELSE
                   IF TV-SCAL1 = "X"    OR TV-SCAL2
                        ADD 01          TO TV-COUNT
                     ELSE
                        IF RADIO-SCAL1  = "X"   OR RADIO-SCAL2 = "X"
                            ADD 01      TO RADIO-COUNT
                        END-IF
                    END-IF
               END-IF
           END-IF.

      *CALCULATING THE AVERAGE OF RATING.........
           COMPUTE WS-OUT-AVAG          = OUT-COUNT / WS-SURVEY-COUNT.
           COMPUTE WS-MOVIE-AVAG        = MOVIES-COUNT/WS-SURVEY-COUNT.
           COMPUTE WS-TV-AVAG           = TV-COUNT / WS-SURVEY-COUNT.
           COMPUTE WS-RADIO-AVAG        = RADIO-COUNT/WS-SURVEY-COUNT.

       GA000-VALIDATIONS               SECTION.
      *TEXT FIELDS CAN'T BE BLANK....
           IF WS-ID = SPACES
              MOVE "MISSING ID"        TO ERR-MESSAGE
           END-IF.

           IF WS-NAME                  = SPACES
              MOVE "SURNAME MISSING"   TO ERR-MESSAGE
           END-IF.


           IF WS-FIRST-NAMES           = SPACES
              MOVE "NAMES MISSING"     TO ERR-MESSAGE
           END-IF.

           IF WS-CONTACT-NUMBER        = SPACES
              MOVE "CELLPHONE MISSING" TO ERR-MESSAGE
           END-IF.

           IF WS-DATE = SPACES
              MOVE "DATE MISSING"      TO ERR-MESSAGE
            END-IF

           PERFORM IA000-AGE-VALIDATION.

       IA000-AGE-VALIDATION            SECTION.
      *CHECKING IF THE AGE IS LESS THAN 5 OR GREATER THAN 120...
           IF WS-AGE < 5 OR WS-AGE > 120
              MOVE "AGE CAN NOT BE LESS THAN 5 OR GREATER THAN 120"
                                        TO ERR-MESSAGE.

       ZA000-READ-FILE                  SECTION.
      *READ THE DATA OFTHE SURVEY TO DO CALCULATIONS......
           READ SURVEY-DATA             NEXT RECORD
              AT END
                 MOVE HIGH-VALUES       TO WS-EOF.
