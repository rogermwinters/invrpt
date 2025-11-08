       IDENTIFICATION DIVISION.
       PROGRAM-ID.   INVRPT.
       AUTHOR. ROGER WINTERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT INVENT-INPUT-FILE  ASSIGN TO 'INVIN'.

            SELECT INVENT-OUTPUT-FILE ASSIGN TO 'INVOUT'.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-INPUT-FILE RECORDING MODE IS F.
       01  FILLER                  PIC X(80).

       FD  INVENT-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE     PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-INVENT-WS               PIC X(3)        VALUE 'NO'.
           05  TOTAL-RECORDS-WS            PIC 999         VALUE ZERO.
           05  TOTAL-DOLLARS-WS            PIC S9(8)V99    VALUE ZERO.
           05  NUMBER-LINES-PER-PAGE-WS    PIC 99          VALUE ZERO.

           05  HOLD-DATE-WS.
               10  HOLD-YR-WS          PIC XXXX.
               10  HOLD-MO-WS          PIC XX.
               10  HOLD-DY-WS          PIC XX.

       01  INVENT-INPUT-RECORD.
           05  REC-TYPE-IN         PIC X.
           05  BATCH-NUM-IN        PIC XX.
           05  SUPPLIER-NUM-IN     PIC X(5).
           05  VOUCHER-NUM-IN      PIC X(6).
           05  INVOICE-NUM-IN      PIC X(8).
           05  ACC-NUM-IN          PIC X(4).
           05  STORE-NUM-IN        PIC XXX.
           05  DATE-IN             PIC X(8).
           05  FILLER              PIC X(12).
           05  AMOUNT-IN           PIC S9(6)V99.
           05  SUPPLIER-NAME-IN    PIC X(23).

       01  DETAILED-OUTPUT-LINE-SETUP.
           05  REC-TYPE-OUT        PIC X.
           05  FILLER              PIC X(10)      VALUE SPACE.
           05  DATE-OUT            PIC XX/XX/XXXX.
           05  FILLER              PIC X(5)       VALUE SPACE.
           05  AMOUNT-OUT          PIC $$$$,$$9.99BCR.
           05  FILLER              PIC X(4)       VALUE SPACE.
           05  ACC-NUM-OUT         PIC X(4).
           05  FILLER              PIC X(5)       VALUE SPACE.
           05  INVOICE-NUM-OUT     PIC X(8).
           05  FILLER              PIC X(2)       VALUE SPACE.
           05  BATCH-NUM-OUT       PIC XX.
           05  FILLER              PIC X(7)       VALUE SPACE.
           05  VOUCHER-NUM-OUT     PIC X(6).
           05  FILLER              PIC X(6)       VALUE SPACE.
           05  STORE-NUM-OUT       PIC XXX.
           05  FILLER              PIC X(8)       VALUE SPACE.
           05  SUPPLIER-NUM-OUT    PIC X(5).
           05  FILLER              PIC X(7)       VALUE SPACE.
           05  SUPPLIER-NAME-OUT   PIC X(23).

       01  TOTAL-RECORDS-LINE-SETUP.
           05  FILLER              PIC X(5)       VALUE SPACE.
           05  FILLER              PIC X(35)      VALUE
                               'NUMBER OF RECORDS PROCESSED IS:'.
           05  TOTAL-RECORDS-OUT   PIC ZZZ9.

       01  TOTAL-DOLLARS-LINE-SETUP.
           05  FILLER              PIC X(5)       VALUE SPACE.
           05  FILLER              PIC X(35)      VALUE
                               'TOTAL NET DOLLAR AMOUNT IS:'.
           05  TOTAL-DOLLARS-OUT        PIC $$$$,$$$,$$9.99BCR.

       01  REPORT-HEADER.
           05  FILLER              PIC X(10)      VALUE 'RUN DATE: '.
           05  HEADER-MO-OUT       PIC 99.
           05                      PIC X          VALUE '/'.
           05  HEADER-DY-OUT       PIC 99.
           05                      PIC X          VALUE '/'.
           05  HEADER-YR-OUT       PIC 9999.
           05  FILLER              PIC X(10)      VALUE SPACE.
           05  FILLER              PIC X(35)       VALUE
                               'INVENTORY REPORT FOR ROGER WINTERS'.

       01  COLUMN-HEADER1.
           05                      PIC X(6)       VALUE 'RECORD'.
           05                      PIC X(5)       VALUE SPACE.
           05                      PIC X(11)      VALUE 'TRANSACTION'.
           05                      PIC X(9)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'DOLLAR'.
           05                      PIC X(7)       VALUE SPACE.
           05                      PIC X(7)       VALUE 'ACCOUNT'.
           05                      PIC X(2)       VALUE SPACE.
           05                      PIC X(7)       VALUE 'INVOICE'.
           05                      PIC X(3)       VALUE SPACE.
           05                      PIC X(5)       VALUE 'BATCH'.
           05                      PIC X(4)       VALUE SPACE.
           05                      PIC X(7)       VALUE 'VOUCHER'.
           05                      PIC X(5)       VALUE SPACE.
           05                      PIC X(5)       VALUE 'STORE'.
           05                      PIC X(6)       VALUE SPACE.
           05                      PIC X(8)       VALUE 'SUPPLIER'.
           05                      PIC X(4)       VALUE SPACE.
           05                      PIC X(8)       VALUE 'SUPPLIER'.

       01  COLUMN-HEADER2.
           05                      PIC X(4)       VALUE 'TYPE'.
           05                      PIC X(7)       VALUE SPACE.
           05                      PIC X(4)       VALUE 'DATE'.
           05                      PIC X(16)      VALUE SPACE.
           05                      PIC X(6)       VALUE 'AMOUNT'.
           05                      PIC X(7)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(3)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(4)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(3)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(6)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(5)       VALUE SPACE.
           05                      PIC X(6)       VALUE 'NUMBER'.
           05                      PIC X(6)       VALUE SPACE.
           05                      PIC X(4)       VALUE 'NAME'.


       PROCEDURE DIVISION.

       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-INVENT-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT INVENT-INPUT-FILE
               OUTPUT INVENT-OUTPUT-FILE
           PERFORM 250-READ-RECORD

           MOVE FUNCTION CURRENT-DATE TO HOLD-DATE-WS

           MOVE HOLD-MO-WS TO HEADER-MO-OUT
           MOVE HOLD-DY-WS TO HEADER-DY-OUT
           MOVE HOLD-YR-WS TO HEADER-YR-OUT

           PERFORM 500-HEADER.

       250-READ-RECORD.
           READ INVENT-INPUT-FILE INTO INVENT-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-INVENT-WS
           END-READ.

       300-PROCESS.
           MOVE    REC-TYPE-IN      TO  REC-TYPE-OUT
           MOVE    DATE-IN          TO  DATE-OUT
           MOVE    AMOUNT-IN        TO  AMOUNT-OUT
           MOVE    ACC-NUM-IN       TO  ACC-NUM-OUT
           MOVE    INVOICE-NUM-IN   TO  INVOICE-NUM-OUT
           MOVE    BATCH-NUM-IN     TO  BATCH-NUM-OUT
           MOVE    VOUCHER-NUM-IN   TO  VOUCHER-NUM-OUT
           MOVE    STORE-NUM-IN     TO  STORE-NUM-OUT
           MOVE    SUPPLIER-NUM-IN  TO  SUPPLIER-NUM-OUT
           MOVE    SUPPLIER-NAME-IN TO  SUPPLIER-NAME-OUT

           ADD 1           TO  TOTAL-RECORDS-WS
           ADD AMOUNT-IN   TO  TOTAL-DOLLARS-WS

           MOVE    DETAILED-OUTPUT-LINE-SETUP TO PRINT-A-SINGLE-LINE
           WRITE   PRINT-A-SINGLE-LINE AFTER ADVANCING 1 LINE

           ADD 1 TO NUMBER-LINES-PER-PAGE-WS

           IF NUMBER-LINES-PER-PAGE-WS >= 15
               PERFORM 500-HEADER
           END-IF

           PERFORM 250-READ-RECORD.

       500-HEADER.
           MOVE REPORT-HEADER TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER PAGE

           MOVE COLUMN-HEADER1 TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 2 LINES

           MOVE COLUMN-HEADER2 TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE

           MOVE SPACES TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER 1 LINE

           MOVE 0 TO NUMBER-LINES-PER-PAGE-WS.

       900-CLOSE.
           MOVE    TOTAL-RECORDS-WS    TO  TOTAL-RECORDS-OUT
           MOVE    TOTAL-DOLLARS-WS    TO  TOTAL-DOLLARS-OUT

           MOVE    TOTAL-RECORDS-LINE-SETUP TO  PRINT-A-SINGLE-LINE
           WRITE   PRINT-A-SINGLE-LINE AFTER 3 LINES

           MOVE    TOTAL-DOLLARS-LINE-SETUP    TO  PRINT-A-SINGLE-LINE
           WRITE   PRINT-A-SINGLE-LINE AFTER 1 LINE

           CLOSE   INVENT-INPUT-FILE
                   INVENT-OUTPUT-FILE.
