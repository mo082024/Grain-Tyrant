       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAIN-SIM.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRAIN PIC 9(5).
       01 PEOPLE PIC 9(3).
       01 GOLD PIC 9(3).
       01 YEAR PIC 9(4).
       01 SEASON PIC X(6).
       01 CONTINUE-GAME PIC X.
       01 GRAIN-TO-SOW PIC 9(4).
       01 GRAIN-HARVESTED PIC 9(5).
       01 HARVEST-MULTIPLIER PIC 9V99 VALUE 2.50.
       01 GRAIN-PER-PERSON PIC 9(3) VALUE 1.
       01 GRAIN-CONSUMED PIC 9(5).
       01 STARVED PIC 9(3).
       01 SOW-RATE-PER-PERSON PIC 9(3) VALUE 10.
       01 HARVEST-RATE-PER-PERSON PIC 9(3) VALUE 20.
       01 MAX-SOWABLE PIC 9(5).
       01 MAX-HARVESTABLE PIC 9(5).
       01 STAT-LINE.
           05 SL-YEAR PIC 9(4).
           05 FILLER PIC X(2) VALUE SPACES.
           05 SL-SEASON PIC X(6).
           05 FILLER PIC X(4) VALUE SPACES.
           05 SL-PEOPLE PIC ZZZ9.
           05 FILLER PIC X(2) VALUE SPACES.
           05 SL-GRAIN PIC ZZZZ9.
           05 FILLER PIC X(2) VALUE SPACES.
           05 SL-GOLD PIC ZZZ9.
           05 FILLER PIC X(1) VALUE SPACES.
           05 SL-ACTION PIC X(30).
       
       PROCEDURE DIVISION.
           PERFORM INITIALIZE-GAME
           PERFORM DISPLAY-HEADER
           PERFORM GAME-LOOP 
               UNTIL CONTINUE-GAME = 'N' OR CONTINUE-GAME = 'n'
           DISPLAY "Game Over. Final year: " YEAR
           STOP RUN.

       INITIALIZE-GAME.
           MOVE 1000 TO GRAIN
           MOVE 100 TO PEOPLE
           MOVE 10 TO GOLD
           MOVE 1 TO YEAR
           MOVE 'Y' TO CONTINUE-GAME.

       DISPLAY-HEADER.
           DISPLAY "Year  Season  People  Grain  Gold Action"
           DISPLAY "----  ------  ------  -----  ---- ------".

       GAME-LOOP.
           PERFORM SPRING-PHASE
           PERFORM SUMMER-PHASE
           PERFORM AUTUMN-PHASE
           PERFORM WINTER-PHASE
           ADD 1 TO YEAR
           DISPLAY "Continue to next year? (Y/N)"
           ACCEPT CONTINUE-GAME.

       SPRING-PHASE.
           MOVE "SPRING" TO SEASON
           PERFORM DISPLAY-STATS
           COMPUTE MAX-SOWABLE = PEOPLE * SOW-RATE-PER-PERSON
           DISPLAY "How much grain to sow? (Max: " MAX-SOWABLE ")"
           ACCEPT GRAIN-TO-SOW
           IF GRAIN-TO-SOW > MAX-SOWABLE
               DISPLAY "Can't sow that much. Sowing max amount."
               MOVE MAX-SOWABLE TO GRAIN-TO-SOW
           END-IF
           IF GRAIN-TO-SOW > GRAIN
               DISPLAY "Not enough grain. Sowing all available grain."
               MOVE GRAIN TO GRAIN-TO-SOW
           END-IF
           SUBTRACT GRAIN-TO-SOW FROM GRAIN
           MOVE FUNCTION CONCATENATE("Sowed: ", GRAIN-TO-SOW, " grain")
               TO SL-ACTION
           PERFORM DISPLAY-STATS.

       SUMMER-PHASE.
           MOVE "SUMMER" TO SEASON
           PERFORM CONSUME-GRAIN
           MOVE FUNCTION CONCATENATE("Consumed: ", GRAIN-CONSUMED, 
               " grain") TO SL-ACTION
           PERFORM DISPLAY-STATS.

       AUTUMN-PHASE.
           MOVE "AUTUMN" TO SEASON
           PERFORM DISPLAY-STATS
           COMPUTE MAX-HARVESTABLE = PEOPLE * HARVEST-RATE-PER-PERSON
           COMPUTE GRAIN-HARVESTED = FUNCTION MIN(
               GRAIN-TO-SOW * HARVEST-MULTIPLIER,
               MAX-HARVESTABLE)
           ADD GRAIN-HARVESTED TO GRAIN
           MOVE FUNCTION CONCATENATE("Harvested: ", GRAIN-HARVESTED, 
               " grain") TO SL-ACTION
           PERFORM DISPLAY-STATS.

       WINTER-PHASE.
           MOVE "WINTER" TO SEASON
           PERFORM CONSUME-GRAIN
           MOVE FUNCTION CONCATENATE("Consumed: ", GRAIN-CONSUMED, 
               " grain") TO SL-ACTION
           PERFORM DISPLAY-STATS.

       CONSUME-GRAIN.
           COMPUTE GRAIN-CONSUMED = PEOPLE * GRAIN-PER-PERSON
           IF GRAIN-CONSUMED > GRAIN
               COMPUTE STARVED = FUNCTION INTEGER(
                   (GRAIN-CONSUMED - GRAIN) / GRAIN-PER-PERSON)
               SUBTRACT STARVED FROM PEOPLE
               MOVE GRAIN TO GRAIN-CONSUMED
               MOVE 0 TO GRAIN
               MOVE FUNCTION CONCATENATE("Starved: ", STARVED, 
                   " people") TO SL-ACTION
           ELSE
               SUBTRACT GRAIN-CONSUMED FROM GRAIN
               MOVE 0 TO STARVED
           END-IF.

       DISPLAY-STATS.
           MOVE YEAR TO SL-YEAR
           MOVE SEASON TO SL-SEASON
           MOVE PEOPLE TO SL-PEOPLE
           MOVE GRAIN TO SL-GRAIN
           MOVE GOLD TO SL-GOLD
           DISPLAY STAT-LINE.
