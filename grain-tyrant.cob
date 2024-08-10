       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAIN-TYRANT.
       
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
       01 POTENTIAL-IMMIGRANTS PIC 9(3).
       01 ACCEPTED-IMMIGRANTS PIC 9(3).
       01 GRAIN-SURPLUS PIC S9(5).
       01 RANDOM-NUM PIC 9(3).
       01 TOTAL-GRAIN-PRODUCED PIC 9(6) VALUE 0.
       01 TOTAL-STARVED PIC 9(5) VALUE 0.
       01 FINAL-SCORE PIC 9(6).
       01 GRAIN-PRICE PIC 9V99.
       01 GRAIN-TO-SELL PIC 9(5).
       01 GOLD-EARNED PIC 9(3).
       01 GOLD-NEEDED PIC 9(3).
       01 GOLD-SHORTAGE PIC 9(3).
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
           PERFORM DISPLAY-INTRO-SCREEN
           PERFORM INITIALIZE-GAME
           PERFORM DISPLAY-HEADER
           PERFORM GAME-LOOP 
               UNTIL CONTINUE-GAME = 'N' OR CONTINUE-GAME = 'n'
               OR PEOPLE = 0
           PERFORM DISPLAY-GAME-OVER-SCREEN
           STOP RUN.

       DISPLAY-INTRO-SCREEN.
           DISPLAY "************************************"
           DISPLAY "*         GRAIN TYRANT             *"
           DISPLAY "*                                  *"
           DISPLAY "* Because World Domination Starts  *"
           DISPLAY "*          With Wheat              *"
           DISPLAY "************************************"
           DISPLAY " "
           DISPLAY "Welcome, aspiring Grain Tyrant!"
           DISPLAY "In this thrilling saga of"
           DISPLAY "agricultural despotism, you'll"
           DISPLAY "navigate the treacherous fields of"
           DISPLAY "crop management and population"
           DISPLAY "control."
           DISPLAY " "
           DISPLAY "RULES:"
           DISPLAY "1. Sow grain in spring, harvest in"
           DISPLAY "   autumn."
           DISPLAY "2. Feed your subjects in summer &"
           DISPLAY "   winter."
           DISPLAY "3. Sell grain for gold to fill your"
           DISPLAY "   royal coffers."
           DISPLAY "4. Survive winter by having enough"
           DISPLAY "   gold."
           DISPLAY "5. Attract immigrants with your"
           DISPLAY "   bountiful grain surplus."
           DISPLAY " "
           DISPLAY "AIM:"
           DISPLAY "Achieve the highest score possible"
           DISPLAY "by balancing population, grain"
           DISPLAY "production, and gold accumulation."
           DISPLAY "Remember, a starving populace makes"
           DISPLAY "for poor subjects!"
           DISPLAY " "
           DISPLAY "Press ENTER to begin your"
           DISPLAY "tyrannical reign!"
           ACCEPT CONTINUE-GAME.

       INITIALIZE-GAME.
           MOVE 1000 TO GRAIN
           MOVE 100 TO PEOPLE
           MOVE 100 TO GOLD
           MOVE 1 TO YEAR
           MOVE 'Y' TO CONTINUE-GAME.

       DISPLAY-HEADER.
           DISPLAY "Year  Season  People  Grain  Gold"
           DISPLAY "----  ------  ------  -----  ----".

       GAME-LOOP.
           PERFORM SPRING-PHASE
           PERFORM SUMMER-PHASE
           PERFORM AUTUMN-PHASE
           PERFORM SELL-GRAIN-PHASE
           PERFORM WINTER-PHASE
           PERFORM IMMIGRATION-PHASE
           IF PEOPLE > 0
               ADD 1 TO YEAR
               DISPLAY "Continue to next year? (Y/N)"
               ACCEPT CONTINUE-GAME
           ELSE
               MOVE "N" TO CONTINUE-GAME
               DISPLAY "Your population has reached 0."
               DISPLAY "Game Over."
           END-IF.

       SPRING-PHASE.
           MOVE "SPRING" TO SEASON
           PERFORM DISPLAY-STATS
           COMPUTE MAX-SOWABLE = PEOPLE * SOW-RATE-PER-PERSON
           DISPLAY "How much grain to sow? (Max: " 
               MAX-SOWABLE ")"
           ACCEPT GRAIN-TO-SOW
           IF GRAIN-TO-SOW > MAX-SOWABLE
               DISPLAY "Can't sow that much. Sowing max."
               MOVE MAX-SOWABLE TO GRAIN-TO-SOW
           END-IF
           IF GRAIN-TO-SOW > GRAIN
               DISPLAY "Not enough grain. Sowing all."
               MOVE GRAIN TO GRAIN-TO-SOW
           END-IF
           SUBTRACT GRAIN-TO-SOW FROM GRAIN
           MOVE FUNCTION CONCATENATE(
               "Sowed: ", GRAIN-TO-SOW, " grain")
               TO SL-ACTION
           PERFORM DISPLAY-STATS.

       SUMMER-PHASE.
           MOVE "SUMMER" TO SEASON
           PERFORM CONSUME-GRAIN
           MOVE FUNCTION CONCATENATE(
               "Consumed: ", GRAIN-CONSUMED, " grain") 
               TO SL-ACTION
           PERFORM DISPLAY-STATS.

       AUTUMN-PHASE.
           MOVE "AUTUMN" TO SEASON
           PERFORM DISPLAY-STATS
           COMPUTE MAX-HARVESTABLE = 
               PEOPLE * HARVEST-RATE-PER-PERSON
           COMPUTE GRAIN-HARVESTED = FUNCTION MIN(
               GRAIN-TO-SOW * HARVEST-MULTIPLIER,
               MAX-HARVESTABLE)
           ADD GRAIN-HARVESTED TO GRAIN
           ADD GRAIN-HARVESTED TO TOTAL-GRAIN-PRODUCED
           MOVE FUNCTION CONCATENATE(
               "Harvested: ", GRAIN-HARVESTED, " grain") 
               TO SL-ACTION
           PERFORM DISPLAY-STATS.

       SELL-GRAIN-PHASE.
           MOVE "SELL" TO SEASON
           PERFORM DISPLAY-STATS
           COMPUTE GRAIN-PRICE = 
               (FUNCTION RANDOM * 0.5) + 0.5
           DISPLAY "Current grain price: " 
               GRAIN-PRICE " gold per grain"
           DISPLAY "How much grain to sell?"
           ACCEPT GRAIN-TO-SELL
           IF GRAIN-TO-SELL > GRAIN
               DISPLAY "Not enough grain. Selling all."
               MOVE GRAIN TO GRAIN-TO-SELL
           END-IF
           COMPUTE GOLD-EARNED = GRAIN-TO-SELL * GRAIN-PRICE
           ADD GOLD-EARNED TO GOLD
           SUBTRACT GRAIN-TO-SELL FROM GRAIN
           MOVE FUNCTION CONCATENATE(
               "Sold: ", GRAIN-TO-SELL, 
               " grain for ", GOLD-EARNED, " gold") 
               TO SL-ACTION
           PERFORM DISPLAY-STATS.

       WINTER-PHASE.
           MOVE "WINTER" TO SEASON
           PERFORM CONSUME-GRAIN
           COMPUTE GOLD-NEEDED = PEOPLE
           IF GOLD < GOLD-NEEDED
               COMPUTE GOLD-SHORTAGE = GOLD-NEEDED - GOLD
               COMPUTE STARVED = 
                   FUNCTION MIN(GOLD-SHORTAGE, PEOPLE)
               SUBTRACT STARVED FROM PEOPLE
               ADD STARVED TO TOTAL-STARVED
               MOVE 0 TO GOLD
               MOVE FUNCTION CONCATENATE(
                   "Starved: ", STARVED, 
                   " people (gold shortage)") TO SL-ACTION
               IF PEOPLE <= 0
                   MOVE 0 TO PEOPLE
                   DISPLAY "Everyone has starved!"
               END-IF
           ELSE
               SUBTRACT GOLD-NEEDED FROM GOLD
               MOVE FUNCTION CONCATENATE(
                   "Paid: ", GOLD-NEEDED, 
                   " gold for winter") TO SL-ACTION
           END-IF
           PERFORM DISPLAY-STATS.

       IMMIGRATION-PHASE.
           MOVE "IMMIGR" TO SEASON
           COMPUTE GRAIN-SURPLUS = 
               GRAIN - (PEOPLE * GRAIN-PER-PERSON)
           COMPUTE RANDOM-NUM = FUNCTION RANDOM * 100 + 1
           COMPUTE POTENTIAL-IMMIGRANTS = RANDOM-NUM + 
               FUNCTION MAX(0, GRAIN-SURPLUS / 100)
           DISPLAY "Potential immigrants: " 
               POTENTIAL-IMMIGRANTS
           DISPLAY "How many immigrants to accept?"
           ACCEPT ACCEPTED-IMMIGRANTS
           IF ACCEPTED-IMMIGRANTS > POTENTIAL-IMMIGRANTS
               MOVE POTENTIAL-IMMIGRANTS TO ACCEPTED-IMMIGRANTS
           END-IF
           ADD ACCEPTED-IMMIGRANTS TO PEOPLE
           MOVE FUNCTION CONCATENATE(
               "Accepted: ", ACCEPTED-IMMIGRANTS, 
               " immigrants") TO SL-ACTION
           PERFORM DISPLAY-STATS.

       CONSUME-GRAIN.
           COMPUTE GRAIN-CONSUMED = PEOPLE * GRAIN-PER-PERSON
           IF GRAIN-CONSUMED > GRAIN
               COMPUTE STARVED = FUNCTION INTEGER(
                   (GRAIN-CONSUMED - GRAIN) / GRAIN-PER-PERSON)
               SUBTRACT STARVED FROM PEOPLE
               ADD STARVED TO TOTAL-STARVED
               MOVE GRAIN TO GRAIN-CONSUMED
               MOVE 0 TO GRAIN
               MOVE FUNCTION CONCATENATE(
                   "Starved: ", STARVED, " people") 
                   TO SL-ACTION
               IF PEOPLE <= 0
                   MOVE 0 TO PEOPLE
                   DISPLAY "Everyone has starved!"
               END-IF
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

       DISPLAY-GAME-OVER-SCREEN.
           DISPLAY "************************************"
           DISPLAY "*           GAME OVER              *"
           DISPLAY "************************************"
           DISPLAY "Final Year: " YEAR
           DISPLAY "Final Population: " PEOPLE
           DISPLAY "Final Grain: " GRAIN
           DISPLAY "Final Gold: " GOLD
           DISPLAY "Total Grain Produced: " 
               TOTAL-GRAIN-PRODUCED
           DISPLAY "Total People Starved: " TOTAL-STARVED
           COMPUTE FINAL-SCORE = 
               (PEOPLE * 10) + (GRAIN / 10) + (GOLD * 100) - 
               (TOTAL-STARVED * 5)
           DISPLAY "FINAL SCORE: " FINAL-SCORE
           DISPLAY "************************************"
           DISPLAY "Score calculation:"
           DISPLAY "(Population * 10) + (Grain / 10) +"
           DISPLAY "(Gold * 100) - (Total Starved * 5)"
           DISPLAY "************************************".
