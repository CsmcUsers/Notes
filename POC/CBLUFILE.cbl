       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CBLUFILE.                                        
      *REMARKS.        FILE TRANSFER BOK --->  CB.
      ***981125應民國１００年更改ACIF-LEN INT-LEN                     
      ***              85/10/01 :
990525***新增外幣機關專戶異動檔案傳送                                 
A91218***新增國庫機關專戶網路銀行業務通報業務                         
       ENVIRONMENT     DIVISION.
      ***
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  WMF01-AMT-04                    PIC  S9(13)V99    VALUE +0.  
       77  WS-BIF-BAL                      PIC  S9(13)V99    VALUE +0.  
       77  WMF01-AMT-05                    PIC  S9(13)V99    VALUE +0.  
       77  WMF032-AMT                      PIC  S9(13)V99    VALUE +0.  
       77  WMF032-NONINT-TOT               PIC  S9(13)V99    VALUE +0.  
       77  SMF032-NONINT-TOT               PIC  S9(13)V99    VALUE +0.  
       77  WMF032-TOT                      PIC  S9(13)       VALUE +0.  
       77  WMF032-TOTAL                    PIC  S9(13)V99    VALUE +0.  
       77  WMF032-REBACK-AMT               PIC  S9(13)V99    VALUE +0.  
950327 77  WMF033-DATA-CNT                 PIC  9(7)       VALUE 0.     
950327 77  WMF031-DATA-DATE                PIC  9(5)       VALUE 0.     
950327 77  WMF031-SECTION                  PIC  9(1)       VALUE 0.     
       77  WMF032-FLAG                     PIC  X(03)      VALUE 'ON '. 
       77  WMF032-FLAG1                    PIC  X(03)      VALUE 'ON '. 
       77  WKX-DD                          PIC  9(02)      VALUE 0.     
       77  SMF032-DD                       PIC  9(02)      VALUE 0.     
       77  WK-CHK-DD                       PIC  9(02)      VALUE 0.     
       77  W-LEN                           PIC  9(4).                   
981125 01  WK-TXN-DATE.                                                 
         10  WK-TXN-YY                     PIC 9(3).                    
         10  WK-TXN-MM                     PIC 9(2).                    
         10  WK-TXN-DD                     PIC 9(2).                    
       01  WKS-END-DATE.                                                
         10  WKS-END-YY                    PIC 9(3).                    
         10  WKS-END-MM                    PIC 9(2).                    
         10  WKS-END-DD                    PIC 9(2).                    
       01  WKS-TXN-DATE.                                                
         10  WKS-DATA-DATE                 PIC 9(5).                    
         10  WKS-DD                        PIC 9(2).                    
       01  WK-IFTS-DATA.
         05  WK-SYSTEM-ID                PIC 9(01)  VALUE 0.
         05  WK-FILE-ID                  PIC X(08)  VALUE SPACES.
         05  WK-COMPRESS-ID              PIC X(08)  VALUE SPACES.
       01  WK-IS-TIME                    PIC S9(06) COMP-3 VALUE +0.    
       01  WK-032-KEY.                                                  
          05  WK-032-KEY-YY              PIC  9(3).                     
          05  WK-032-KEY-MM              PIC  9(2).                     
          05  WK-032-KEY-BRANCH          PIC  X(7).                     
          05  WK-032-KEY-DD              PIC  9(2).                     
       01  WK-032-DATE.                                                 
          05  WK-032-YY                  PIC  9(3).                     
          05  WK-032-MM                  PIC  9(2).                     
          05  WK-032-DD                  PIC  9(2).                     
       01  TP-032-DATE                   PIC X(5) VALUE SPACE.          
       01  WS-032-DATE.                                                 
          05  WS-032-YY                  PIC  9(3).                     
          05  WS-032-MM                  PIC  9(2).                     
981125 01  INT-LENGTH               PIC S9(4) COMP VALUE +600.          
       01  WK-LEN                        PIC S9(4) COMP VALUE +17.      
       01  WK-AREA.
         05  FILLER                      PIC X(10)  VALUE '//WK-PARA/'.
         05  WK-PARA                     PIC X(10)  VALUE SPACES.
         05  WK-MSG-CODE                 PIC X(4)   VALUE '    '.       
         05  WK-MSG-CONTENT              PIC X(40)  VALUE SPACES.       
         05  WK-DATA.                                                   
           10  CTF-BIF-BAL-G             PIC   X(15).                   
           10  CTF-BIF-BAL-T       REDEFINES  CTF-BIF-BAL-G.            
             15  CTF-BIF-BAL-M           PIC X(01).                     
             15  WCTF-BIF-BAL            PIC 9(12)V99.                  
       01  FILLER                          PIC X(10) VALUE '/IDX-ARE/'. 
       01  IDX-G.                                                       
           05  IDX1                        PIC 9(4) COMP.               
             88  IDX1-B5                   VALUE  18 46 52.             
             88  IDX1-B6                   VALUE  9.                    
           05  IDX2                        PIC 9(4) COMP.               
           05  IDX3                        PIC 9(4) COMP.               
           05  IDX-BYTE-BEG                PIC 9(4) COMP.               
           05  IDX-BYTE-END                PIC 9(4) COMP.               
       01  WK-CHIN.                                                     
           05  WK-REC-LEN                  PIC 9(9) COMP-4.             
           05  WK-CDC-LEN-G                PIC 9(9) COMP-4.             
           05  WK-CDC-LEN-R                REDEFINES WK-CDC-LEN-G.      
             10 FILLER                     PIC X(2).                    
             10 WK-CDC-LEN                 PIC X(2).                    
       01  FILLER                        PIC X(10)  VALUE '//TPSMF01/'. 
       01  TPSMF01                         PIC  X(85)  VALUE SPACES.    
       01  FILLER                        PIC X(10)  VALUE '/TPSMF032/'. 
       01  TPSMF032                        PIC  X(58)  VALUE SPACES.    
       01  FILLER                        PIC X(10)  VALUE '/TTSMF032/'. 
       01  TTSMF032                        PIC  X(58)  VALUE SPACES.    
      *                                                                 
       01  FILLER                        PIC X(10)  VALUE '//WS-MF01/'. 
       01  WMF01.                                                       
         05  WMF01-KEY.                                                 
7          10  WMF01-MAIN-BR                 PIC   9(07).               
14         10  WMF01-SUB-BR                  PIC   9(07).               
21         10  WMF01-DATA-DATE               PIC   9(07).               
28         10  WMF01-STAN-NO                 PIC   9(07).               
31         10  WMF01-ACCOUNT-YEAR            PIC   9(03).               
33         10  WMF01-INVOICE-CODE            PIC   X(02).               
39       05  WMF01-INVOICE-NO                PIC   X(06).               
         05  WMF01-INVOICE-NO-R          REDEFINES WMF01-INVOICE-NO     
                                             PIC   9(06).               
40       05  WMF01-RCV-PAY                   PIC   X(01).               
51       05  WMF01-ACCOUNT-CODE              PIC   9(11).               
54       05  WMF01-ACCOUNT-ADD               PIC   X(03).               
55       05  WMF01-AMT-MARK                  PIC   X(01).               
69       05  WMF01-CK-AMT                    PIC   9(12)V99.            
76       05  WMF01-ACCOUNT-ORG               PIC   9(07).               
83       05  WMF01-CHK-ORG                   PIC   9(07).               
85       05  WMF01-CORRECT-TYPE              PIC   X(02).               
       01  TMF01.                                                       
         05  TMF01-KEY.                                                 
7          10  TMF01-MAIN-BR                 PIC   9(07).               
14         10  TMF01-SUB-BR                  PIC   9(07).               
21         10  TMF01-DATA-DATE               PIC   9(07).               
28         10  TMF01-STAN-NO                 PIC   9(07).               
31         10  TMF01-ACCOUNT-YEAR            PIC   9(03).               
33         10  TMF01-INVOICE-CODE            PIC   X(02).               
39       05  TMF01-INVOICE-NO                PIC   X(06).               
40       05  TMF01-RCV-PAY                   PIC   X(01).               
51       05  TMF01-ACCOUNT-CODE              PIC   9(11).               
54       05  TMF01-ACCOUNT-ADD               PIC   X(03).               
55       05  TMF01-AMT-MARK                  PIC   X(01).               
69       05  TMF01-CK-AMT                    PIC   9(12)V99.            
76       05  TMF01-ACCOUNT-ORG               PIC   9(07).               
83       05  TMF01-CHK-ORG                   PIC   9(07).               
85       05  TMF01-CORRECT-TYPE              PIC   X(02).               
       01  RMF01-KEY.                                                   
7        05  RMF01-MAIN-BR                 PIC   9(07).                 
14       05  RMF01-SUB-BR                  PIC   9(07).                 
21       05  RMF01-DATA-DATE               PIC   9(07).                 
28       05  RMF01-STAN-NO                 PIC   9(07).                 
31       05  RMF01-ACCOUNT-YEAR            PIC   9(03).                 
33       05  RMF01-INVOICE-CODE            PIC   X(02).                 
       01  FILLER                        PIC X(10)  VALUE '/CDC-MF01/'. 
       01  CMF01                            PIC   X(85).                
       01  CMF01-R                       REDEFINES CMF01.               
         05  CMF01-DATA-G.                                              
           15  CMF01-DATA                OCCURS 85 TIMES                
                                            PIC X.                      
       01  FILLER                        PIC X(10)  VALUE '/CDC-MF02/'. 
       01  CMF02                            PIC   X(58).                
       01  CMF02-R                       REDEFINES CMF02.               
         05  CMF02-DATA-G.                                              
           15  CMF02-DATA                OCCURS 58 TIMES                
                                            PIC X.                      
       01  FILLER                        PIC X(10)  VALUE '//WMF032//'. 
       01  WMF032.                                                      
         05  WMF032-KEY.                                                
5          10  WMF032-DATA-DATE             PIC   9(05).                
12         10  WMF032-BRANCH-CODE           PIC   9(07).                
14         10  WMF032-DD                    PIC   9(02).                
43       05  WMF032-NONINT-NO               PIC   9(07).                
44       05  WMF032-NONINT-AMT-MARK         PIC   X(01).                
58       05  WMF032-NONINT-AMT              PIC   9(12)V99.             
21       05  WMF032-INT-NO                  PIC   9(07).                
22       05  WMF032-INT-AMT-MARK            PIC   X(01).                
36       05  WMF032-INT-AMT                 PIC   9(12)V99.             
       01  FILLER                        PIC X(10)  VALUE '//TMF032//'. 
       01  TMF032.                                                      
         05  TMF032-KEY.                                                
5          10  TMF032-DATA-DATE             PIC   9(05).                
12         10  TMF032-BRANCH-CODE           PIC   9(07).                
14         10  TMF032-DD                    PIC   9(02).                
43       05  TMF032-NONINT-NO               PIC   9(07).                
44       05  TMF032-NONINT-AMT-MARK         PIC   X(01).                
58       05  TMF032-NONINT-AMT              PIC   9(12)V99.             
21       05  TMF032-INT-NO                  PIC   9(07).                
22       05  TMF032-INT-AMT-MARK            PIC   X(01).                
36       05  TMF032-INT-AMT                 PIC   9(12)V99.             
       01  FILLER                        PIC X(10)  VALUE '//RMF032//'. 
       01  RMF032-KEY.                                                  
5        05  RMF032-DATA-DATE               PIC   9(05).                
12       05  RMF032-BRANCH-CODE             PIC   9(07).                
14       05  RMF032-DD                      PIC   9(02).                
      *---------------------------------------------------*             
       01  WMF033.                                                      
         05  WMF033-KEY.                                                
5          10  WMF033-DATA-DATE              PIC   9(05).               
6          10  WMF033-DATE-SECTION           PIC   9(01).               
13         10  WMF033-BRANCH-CODE            PIC   9(07).               
29         10  WMF033-ACCOUNT-NO             PIC   X(16).               
37       05  WMF033-ORG                      PIC   9(8).                
44       05  WMF033-OPEN-ACC                 PIC   9(07).               
55       05  WMF033-APRV-NO                  PIC   9(11).               
56       05  WMF033-CHAR-CODE                PIC   9(1).                
57       05  WMF033-INT-CODE                 PIC   9(1).                
58       05  WMF033-CT-CODE                  PIC   9(1).                
59       05  WMF033-CURRENCY-CODE            PIC   9(1).                
60       05  WMF033-TXN-CODE                 PIC   9(1).                
140      05  WMF033-NAME                     PIC   X(80).               
       01  FILLER                        PIC X(11)  VALUE '/CDC-MF033/'.
       01  CMF033                           PIC   X(140).               
       01  CMF033-R                       REDEFINES CMF033.             
         05  CMF033-DATA-G.                                             
           15  CMF033-DATA                OCCURS 140 TIMES              
                                            PIC X.                      
       01  RMF033.                                                      
         05  RMF033-KEY.                                                
5          10  RMF033-DATA-DATE              PIC   9(05).               
6          10  RMF033-DATE-SECTION           PIC   9(01).               
13         10  RMF033-BRANCH-CODE            PIC   9(07).               
29         10  RMF033-ACCOUNT-NO             PIC   X(16).               
37       05  RMF033-ORG                      PIC   9(8).                
44       05  RMF033-OPEN-ACC                 PIC   9(07).               
55       05  RMF033-APRV-NO                  PIC   9(11).               
56       05  RMF033-CHAR-CODE                PIC   9(1).                
57       05  RMF033-INT-CODE                 PIC   9(1).                
58       05  RMF033-CT-CODE                  PIC   9(1).                
59       05  RMF033-CURRENCY-CODE            PIC   9(1).                
60       05  RMF033-TXN-CODE                 PIC   9(1).                
140      05  RMF033-NAME                     PIC   X(80).               
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF01/'. 
      *01  TIPMF01                                                      
           COPY                             CBKMF01.                    
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF02/'. 
      *01  TIPMF02                                                      
           COPY                             CBKMF02.                    
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF31/'. 
      *01  TIPMF031                                                     
           COPY                             CBKMF031.                   
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF32/'. 
      *01  TIPMF032                                                     
           COPY                             CBKMF032.                   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF32/'. 
950327*01  TIPMF033                                                     
           COPY                             CBKMF033.                   
      *-------------------------------------------------------------*   
060524 01  FILLER                        PIC X(10)  VALUE '//CBKTPBIF'. 
060524     COPY                             CBKTPBIF.                   
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF51/'. 
      *01  TIPMF051                                                     
           COPY                             CBKMF051.                   
      *-------------------------------------------------------------*   
       01  FILLER                        PIC X(10)  VALUE '//CBKMF52/'. 
      *01  TIPMF052                                                     
           COPY                             CBKMF052.                   
       01  FILLER                          PIC X(10)  VALUE '/CBKLEN/'. 
      *01  LEN.                                                         
           COPY                            CBKLEN.                      
      *-------------------------------------------------------------*   
       01  L86P-DATA.
         05  L86P-REP-ID                 PIC X(06)  VALUE '*OUT* '.
         05  L86P-STAN-NO                PIC X(07).
         05  L86P-BR-CODE                PIC X(09)  VALUE ' BRANCH: '.
         05  L86P-TWA-BR-CODE            PIC X(03).
         05  L86P-FILLER                 PIC X(03)  VALUE SPACES.
         05  L86P-PRO                    PIC X(06)  VALUE ' PRO: '.
         05  L86P-TXN-ID                 PIC X(04).
      ***
      ****  COPY INT CALULATE  RTN PARAMETER  ****                      
       01  FILLER                        PIC X(10)  VALUE '/LNKINT/'.   
981125*01  INT-GROUP COPY LNKINT.                                       
           COPY           LNKINT7.                                      
      ****  COPY INT MNEMONIC ITEM  ****                                
      *01  INT-M  COPY LNKINTM.                                         
           COPY        LNKINTM.                                         
      **                                                                
      ****  COPY DAT MNEMONIC ITEM  ****                                
      * 01  DAT-M  COPY LNKDATM.                                        
            COPY        LNKDATM.                                        
      **                                                                
      *---------------------------------------------------*             
      *     PARA AREA FOR CCDCMAIN                        *             
      *---------------------------------------------------*             
       01  FILLER                          PIC X(10) VALUE '/CDC-ARE/'. 
       01  CDC.                                                         
           05  CDC-CVT-TYPE                PIC X(4).                    
             88 CDC-C-ITS                  VALUE  'ITS '.               
             88 CDC-C-STI                  VALUE  'STI '.               
             88 CDC-C-VALID-TYPE           VALUE  'ITS ' 'STI '.        
           05  CDC-IN-LEN                  PIC 9(9)  COMP-4.            
           05  CDC-OUT-LEN                 PIC 9(9)  COMP-4.            
           05  CDC-MAX-LEN                 PIC 9(9)  COMP-4.            
           05  CDC-SPECIAL-SYMBOL          PIC X(12).                   
           05  CDC-SPECIAL-SYMBOL-R        REDEFINES                    
               CDC-SPECIAL-SYMBOL.                                      
             10 CDC-INVALID-PLANE-NO       PIC 9(04) COMP-4.            
             10 CDC-INVALID-SYMBOL         PIC X(04).                   
             10 CDC-OVER-PLANE-NO          PIC 9(04) COMP-4.            
             10 CDC-OVER-SYMBOL            PIC X(04).                   
           05  CDC-CHINESE-FLAG            PIC X.                       
           05  CDC-ERROR-MSG               PIC X(80).                   
           05  CDC-ERROR-MSG-G             REDEFINES CDC-ERROR-MSG.     
             10 CDC-ERROR-MSG-ID.                                       
               15 CDC-ERROR-MSG-ID-H       PIC X(07).                   
               15 CDC-ERROR-MSG-CLASS      PIC X.                       
             10 CDC-MSG-TEXT               PIC X(72).                   
           05  CDC-RECORD-IN-G.                                         
             10 CDC-RECORD-IN              OCCURS 1 TO 32000 TIMES      
                DEPENDING ON WK-REC-LEN    PIC X.                       
                                                                        
       01  FILLER                          PIC X(10) VALUE '/CDCP   /'. 
       01  CDCP.                                                        
           05  CDC-P-RTRN-CODE             PIC X(1).                    
             88 CDC-C-CVT-NORMAL           VALUE SPACE.                 
             88 CDC-C-CVT-TYP-ERR          VALUE '1'.                   
             88 CDC-C-BUS-COD-ERR          VALUE '2'.                   
             88 CDC-C-CVT-ERR              VALUE '3'.                   
             88 CDC-C-CVT-TABLE-ERR        VALUE '4'.                   
             88 CDC-C-CVT-FISG-DATA-ERR    VALUE '5'.                   
             88 CDC-C-CVT-PROG-ABEND       VALUE '6'.                   
             88 CDC-C-CVT-COMM-LEN-ERR     VALUE '7'.                   
           05  CDC-P-CVT-TYPE              PIC X(4).                    
           05  CDC-P-REC.                                               
             10 CDC-P-LEN                  PIC 9(4) COMP.               
             10 CDC-P-HEADER               PIC X(3).                    
             10 CDC-P-BASIC-DATA.                                       
               15 CDC-P-BASIC-DATA-1.                                   
                 20 CDC-P-MSG-TYPE         PIC X(4).                    
                 20 CDC-P-PROCESS-CODE     PIC X(4).                    
                 20 CDC-P-STAN-NO          PIC X(7).                    
                 20 CDC-P-DEST-BANK        PIC X(7).                    
                 20 CDC-P-SOURCE-BR        PIC X(3).                    
                 20 FILLER                 PIC X(20).                   
               15 CDC-P-MAC-KEY1           PIC X(4).                    
             10 CDC-P-BITMAP               PIC X(8).                    
             10 CDC-P-DATA-G.                                           
               15 CDC-P-DATA               OCCURS 450 TIMES             
                                           PIC X.                       
       01  FILLER                          PIC X(10) VALUE '/CDC-MEM/'. 
       01  CDCM.                                                        
           05  CDC-M-CONVERSION-TYPE.                                   
             10 CDC-M-CVT-ITS              PIC X(04) VALUE 'ITS '.      
             10 CDC-M-CVT-STI              PIC X(04) VALUE 'STI '.      
           05  CDC-M-CHINESE-FLAG-TYPE.                                 
             10 CDC-M-CHINESE-ONLY         PIC X(01) VALUE 'Y'.         
             10 CDC-M-CHINESE-NO           PIC X(01) VALUE 'N'.         
           05  CDC-M-ITS-SPECIAL-SYMBOL.                                
             10 CDC-M-ITS-INVALID-PLANE-N  PIC 9(4) COMP-4 VALUE 1.     
             10 CDC-M-ITS-INVALID-SYMBOL   PIC 9(9) COMP-4 VALUE 8567.  
             10 CDC-M-ITS-OVER-PLANE-NO    PIC 9(4) COMP-4 VALUE 1.     
             10 CDC-M-ITS-OVER-SYMBOL      PIC 9(9) COMP-4 VALUE 8560.  
           05  CDC-M-STI-SPECIAL-SYMBOL.                                
             10 CDC-M-STI-INVALID-PLANE-N  PIC 9(4) COMP-4 VALUE 1.     
             10 CDC-M-STI-INVALID-SYMBOL   PIC 9(9) COMP-4 VALUE 17638. 
             10 CDC-M-STI-OVER-PLANE-NO    PIC 9(4) COMP-4 VALUE 1.     
             10 CDC-M-STI-OVER-SYMBOL      PIC 9(9) COMP-4 VALUE 17640. 
8512M      05  CDC-M-CHI-CODE-TAB.                                      
             10 CDC-M-CHI-0E               PIC X(1)  VALUE ''.         
             10 CDC-M-CHI-0F               PIC X(1)  VALUE ''.         
             10 CDC-M-CHI-21               PIC X(1)  VALUE ' '.         
       01  CONSOLE-DATA.                                                
8          05  OC-CTO-ERR-MSG               PIC X(8) VALUE SPACES.      
9          05  FILLER                       PIC X(1) VALUE SPACE.       
14         05  OC-CTO-BLANK-1               PIC X(5) VALUE 'STAN:'.     
17         05  OC-CTO-SOURCE-BANK           PIC X(3).                   
18         05  FILLER                       PIC X(1) VALUE SPACE.       
25         05  OC-CTO-STAN-NO               PIC X(7).                   
26         05  FILLER                       PIC X(1) VALUE SPACE.       
30         05  OC-CTO-BLANK-2               PIC X(4) VALUE 'MSG:'.      
34         05  OC-CTO-MSG-TYPE              PIC X(4).                   
35         05  FILLER                       PIC X(1) VALUE SPACE.       
39         05  OC-CTO-BLANK-3               PIC X(4) VALUE 'PRO:'.      
43         05  OC-CTO-PROCESS-CODE          PIC X(4).                   
44         05  FILLER                       PIC X(1) VALUE SPACE.       
48         05  OC-CTO-BLANK-4               PIC X(4) VALUE 'CVT:'.      
52         05  OC-CTO-CVT-TYPE              PIC X(4).                   
57         05  OC-CTO-BLANK-5               PIC X(5) VALUE ' -CB-'.     
69         05  OC-CTO-FREE                  PIC X(12) VALUE SPACES.     
       01  SV-WORK.                                                     
           05  SV-SOURCE-BANK               PIC X(3).                   
           05  SV-STAN-NO                   PIC X(7).                   
           05  SV-MSG-TYPE                  PIC X(4).                   
           05  SV-PROCESS-CODE              PIC X(4).                   
       01  FILLER                        PIC X(10)  VALUE '//CBKMSG//'.
      *01  MSG.
           COPY                          CBKMSG.
           COPY                          CBK01.
      ***
       01  FILLER                        PIC X(10)  VALUE '/CBKMSGM//'.
      *01  MSG-M.
           COPY                          CBKMSGM.
      ***
       01  FILLER                        PIC X(10)  VALUE '/ISKTWAM//'.
      *01  TWA-M.
           COPY                          ISKTWAM.
      ***
       LINKAGE SECTION.
      *01  BLLCELLS.                                                    
      *  05  FILLER                      PIC S9(8) COMP.                
      *  05  CWA-PTR                     PIC S9(8) COMP.                
      *  05  TWA-PTR                     PIC S9(8) COMP.                
      *  05  CTF-PTR                     PIC S9(8) COMP.                
      ***
      *01  CWA.
           COPY                          ISKCWA.
      ***
      *01  TWA.
           COPY                          ISKTWA.
      ***
      *01  CTF.
           COPY                          CBKCTF1.                       
      ***
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS-RTN.
           MOVE        '/0000-MAI/'               TO    WK-PARA.
      *    EXEC CICS ADDRESS CWA (CWA-PTR) END-EXEC.                    
           EXEC CICS ADDRESS CWA (ADDRESS OF  CWA) END-EXEC.            
           PERFORM     1000-TXN-INIT-RTN          THRU  1000-EXIT.
           PERFORM     3000-FILE-RETRIV-RTN       THRU  3000-EXIT.
           PERFORM     5000-FILE-REPAIR-RTN       THRU  5000-EXIT.      
           PERFORM     7000-MSG-OUTPUT-RTN        THRU  7000-EXIT.
           PERFORM     8000-TXN-END-RTN           THRU  8000-EXIT.

       0000-EXIT.
           EXIT.
      ***
       1000-TXN-INIT-RTN.
           MOVE        '/1000-INI/'        TO      WK-PARA.
           PERFORM     901-TXN-INIT-RTN    THRU    901-EXIT.
           MOVE        'CBLUFILE'          TO      TWA-MAP-PROG-NAME.   
           MOVE        SPACES              TO      TWA-SAP-PROG-NAME.
           MOVE        TWA-M-AP-NORMAL     TO      TWA-TXN-RETURN-CODE.
       1000-EXIT.
           EXIT.
      ***
       2000-0200-DATA-CHECK-RTN.
           MOVE        '/2000-INP/'        TO      WK-PARA.
      *--- CHECK OPTIONAL INPUT ITEMS ------------------------------*
           EXEC CICS   HANDLE CONDITION                                 
                       NOTOPEN(601-FILE-NOT-OPEN)                       
                       NOTFND(602-NO-REC-FOUND)                         
                       ERROR(603-OTHER-ERROR)                           
                       ENDFILE(604-END-OF-REC)                          
                       DUPREC(605-DUPREC-RTN)                           
                       END-EXEC.                                        

       2000-EXIT.
           EXIT.
      ***
       2999-SEARCH-DATE-RTN.                                            
           MOVE        MF032-DATA-DATE     TO      WKS-DATA-DATE.       
           MOVE        01                  TO      WKS-DD.              
           MOVE        WKS-TXN-DATE        TO      WKS-END-DATE.        
      *----------------END OF MONTH---------------------------------*   
           PERFORM     9171-LN-INT-AREA-CLEAR.                          
           MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE-DATE.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.   
981125     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.       
           MOVE        WKS-END-YY          TO      DAT-P-SPECIAL-YY.    
           MOVE        WKS-END-MM          TO      DAT-P-SPECIAL-MM.    
           MOVE        01                  TO      DAT-P-SPECIAL-DD.    
      *    EXEC CICS ENTER TRACEID(1) FROM(DAT-P-SPECIAL-DATE)          
      *                                                     END-EXEC.   
           MOVE        DAT-M-FUN-8         TO      DAT-P-FUN-CODE.      
981125     EXEC        CICS                LINK    PROGRAM ('NACINT7')  
                       COMMAREA (INT-GROUP)        LENGTH (INT-LENGTH)  
                       END-EXEC.                                        
           PERFORM     9164-LN-DATE-ERR-HDL-RTN.                        
           MOVE        DAT-P-CORR-DATE     TO      WKS-END-DATE.        
           MOVE        DAT-P-CORR-DD       TO      WKX-DD.              
       2999-EXIT.                                                       
           EXIT.                                                        
060524 2300-CLS-BIF-RTN.                                                
           MOVE        ZEROS               TO      BIF-D-KEY.           
           EXEC CICS   READ                                             
                       DATASET('CBTPBIF')                               
                       INTO(TIPBIF-D)                                   
                       RIDFLD(BIF-D-KEY)                                
                       LENGTH(BIF-LENGTH)                               
                       UPDATE                                           
           END-EXEC.                                                    
           IF         BIF-D-TRAN-01-ON                                  
           THEN                                                         
             MOVE      'MB38'               TO      WK-MSG-CODE         
             MOVE      '重傳檔案需先通知央行及解除傳檔控管'           
                                            TO      WK-MSG-CONTENT      
             PERFORM   9999-ERR-MSG-OUT-RTN.                            
      *    ENDIF                                                        
           MOVE        1                    TO      BIF-D-TRAN-01.      
           EXEC CICS   REWRITE                                          
                       DATASET('CBTPBIF')                               
                       FROM(TIPBIF-D)                                   
                       LENGTH(BIF-LENGTH)                               
           END-EXEC.                                                    
       2300-EXIT.                                                       
           EXIT.                                                        
       3000-FILE-RETRIV-RTN.
           IF          CTF-INQ-TYPE        =       1                    
           THEN                                                         
             PERFORM   2300-CLS-BIF-RTN    THRU    2300-EXIT
             PERFORM   3100-MF01-RETRIV-RTN                             
                                           THRU    3100-EXIT.           
      *    ENDIF                                                        
950327*    IF          CTF-INQ-TYPE        =       3                    
      *    THEN                                                         
      *      PERFORM   3200-MF031-RETRIV-RTN                            
      *                                    THRU    3200-EXIT.           
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       4                    
           THEN                                                         
             PERFORM   3400-MF032-RETRIV-RTN                            
                                           THRU    3400-EXIT.           
      *    ENDIF                                                        
950327     IF          CTF-INQ-TYPE        =       7                    
           THEN                                                         
             PERFORM   3500-MF033-RETRIV-RTN                            
                                           THRU    3500-EXIT            
             PERFORM   3510-MF033-RETRIV-RTN                            
                                           THRU    3510-EXIT.           
      *    ENDIF                                                        
                                                                        
       3000-EXIT.                                                       
           EXIT.                                                        
       3100-MF01-RETRIV-RTN.                                            
           EXEC CICS   HANDLE CONDITION                                 
                       ENDFILE(3111-CHECK-DATA-RTN)                     
                       END-EXEC.                                        
           MOVE        ZEROS               TO       CMF01.              
           MOVE        33                  TO       W-LEN.              
           MOVE        SPACES              TO       CDC-RECORD-IN-G.    
           MOVE        SPACES              TO       CDC-ERROR-MSG.      
           MOVE        1                   TO       IDX3.               
           PERFORM     3904-MV-DATA-RTN    THRU     3904-EXIT           
           VARYING     IDX3 FROM  1  BY  1 UNTIL    IDX3 >  W-LEN.      
           PERFORM     4000-CVT-ITS-RTN    THRU     4000-EXIT.          
           PERFORM     3905-MV-DATA-RTN    THRU     3905-EXIT           
           VARYING     IDX3 FROM  1  BY  1 UNTIL    IDX3 >  W-LEN.      
           MOVE        CMF01               TO       RMF01-KEY.          
                                                                        
                                                                        
           EXEC  CICS  STARTBR                                          
                       DATASET('CBMF01')                                
                       RIDFLD(RMF01-KEY)                                
                       GTEQ                                             
           END-EXEC.                                                    
                                                                        
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF01')                                
                       INTO(TPSMF01)                                    
                       LENGTH(MF01-LENGTH)                              
                       RIDFLD(RMF01-KEY)                                
           END-EXEC.                                                    
                                                                        
       3100-MF01-READNEXT-RTN.                                          
           MOVE        TPSMF01             TO       CMF01.              
           MOVE        85                  TO       W-LEN.              
           MOVE        SPACES              TO       CDC-RECORD-IN-G.    
           MOVE        SPACES              TO       CDC-ERROR-MSG.      
           MOVE        1                   TO       IDX3.               
           PERFORM     3904-MV-DATA-RTN    THRU     3904-EXIT           
           VARYING     IDX3 FROM  1  BY  1 UNTIL    IDX3 >  W-LEN.      
           PERFORM     4100-CVT-STI-RTN     THRU    4100-EXIT.          
           PERFORM     3905-MV-DATA-RTN    THRU     3905-EXIT           
           VARYING     IDX3 FROM  1  BY  1 UNTIL    IDX3 >  W-LEN.      
           MOVE        CMF01                TO      TIPMF01.            
                                                                        
           IF          MF01-INVOICE-CODE    =       '04'                
           THEN                                                         
             ADD       MF01-CK-AMT          TO      WMF01-AMT-04.       
      *    ENDIF                                                        
                                                                        
           IF          MF01-INVOICE-CODE    =       '05'                
           THEN                                                         
             ADD       MF01-CK-AMT          TO      WMF01-AMT-05.       
      *    ENDIF                                                        
                                                                        
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF01')                                
                       INTO(TPSMF01)                                    
                       LENGTH(MF01-LENGTH)                              
                       RIDFLD(RMF01-KEY)                                
           END-EXEC.                                                    
                                                                        
           GO    TO    3100-MF01-READNEXT-RTN.                          
       3111-CHECK-DATA-RTN.                                             
           IF         (WMF01-AMT-04    NOT =       WMF01-AMT-05)        
           THEN                                                         
             MOVE     'MB55'              TO      MSG-P-OUT-CODE        
             PERFORM  917-ERR-MSG-OUT-RTN.                              
      *    ENDIF                                                        
                                                                        
       3100-EXIT.                                                       
           EXIT.                                                        
       3200-MF031-RETRIV-RTN.                                           
           EXEC CICS   HANDLE CONDITION                                 
                       ENDFILE(3200-9999-DATA-RTN)                      
                       DUPREC(3200-9999-REW-RTN)                        
                       NOTFND(3200-9999-WRITE-RTN)                      
                       END-EXEC.                                        
           MOVE        SPACES   TO  MF031-KEY.                          
           EXEC  CICS  STARTBR                                          
                       DATASET('CBMF031')                               
                       RIDFLD(MF031-KEY)                                
           END-EXEC.                                                    
       3200-MF031-READNEXT-RTN.                                         
                                                                        
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF031')                               
                       INTO(TIPMF031)                                   
                       LENGTH(MF031-LENGTH)                             
                       RIDFLD(MF031-KEY)                                
           END-EXEC.                                                    
**********避免重作加總                                                
           IF          MF031-BRANCH-CODE =  9999999                     
           THEN                                                         
                       GO    TO    3200-MF031-READNEXT-RTN.             
      *    ENDIF                                                        
           MOVE        MF031-DATA-DATE     TO  WMF031-DATA-DATE.        
           MOVE        MF031-DATE-SECTION  TO  WMF031-SECTION.          
           COMPUTE     WMF032-NONINT-TOT =  WMF032-NONINT-TOT           
                                         +  MF031-NONINT-AMT.           
           COMPUTE     SMF032-NONINT-TOT =  SMF032-NONINT-TOT           
                                         +  MF031-INT-AMT.              
                                                                        
           GO    TO    3200-MF031-READNEXT-RTN.                         
       3200-9999-DATA-RTN.                                              
           EXEC  CICS  READ                                             
                 DATASET('CBMF031')                                     
                 INTO(TIPMF031)                                         
                 LENGTH(MF031-LENGTH)                                   
                 RIDFLD(MF031-KEY)                                      
                 UPDATE                                                 
           END-EXEC.                                                    
           GO          TO    3200-9999-REW-RTN.                         
       3200-9999-WRITE-RTN.                                             
           MOVE        WMF031-DATA-DATE    TO    MF031-DATA-DATE.       
           MOVE        WMF031-SECTION      TO    MF031-DATE-SECTION.    
           MOVE        9999999             TO    MF031-BRANCH-CODE.     
           MOVE        SPACES              TO    MF031-ACCOUNT-NO.      
           MOVE        WMF032-NONINT-TOT   TO    MF031-INT-AMT.         
           MOVE        SMF032-NONINT-TOT   TO    MF031-NONINT-AMT.      
           EXEC  CICS  WRITE                                            
                       DATASET('CBMF031')                               
                       FROM(TIPMF031)                                   
                       LENGTH(MF031-LENGTH)                             
                       RIDFLD(MF031-KEY)                                
           END-EXEC.                                                    
           GO          TO      3200-EXIT.                               
       3200-9999-REW-RTN.                                               
           MOVE        WMF031-DATA-DATE    TO    MF031-DATA-DATE.       
           MOVE        WMF031-SECTION      TO    MF031-DATE-SECTION.    
           MOVE        9999999             TO    MF031-BRANCH-CODE.     
           MOVE        SPACES              TO    MF031-ACCOUNT-NO.      
           MOVE        WMF032-NONINT-TOT   TO    MF031-INT-AMT.         
           MOVE        SMF032-NONINT-TOT   TO    MF031-NONINT-AMT.      
           EXEC  CICS  REWRITE                                          
                       DATASET('CBMF031')                               
                       FROM(TIPMF031)                                   
           END-EXEC.                                                    
           EXEC        CICS    UNLOCK                                   
                       DATASET('CBMF031')                               
           END-EXEC.                                                    
                                                                        
           GO          TO      3200-EXIT.                               
950327 3200-EXIT.                                                       
           EXIT.                                                        
       3400-MF032-RETRIV-RTN.                                           
           EXEC CICS   HANDLE CONDITION                                 
                       ENDFILE(3411-CHECK-DATA-RTN)                     
                       END-EXEC.                                        
      *    MOVE        ZEROS                TO     CMF02.               
981125     MOVE        TWA-TXN-YY7          TO     WK-032-YY.           
           MOVE        TWA-TXN-MM7          TO     WK-032-MM.           
           MOVE        TWA-TXN-DD7          TO     WK-032-DD.           
880106     IF          WK-032-MM            =      01                   
   "       THEN                                                         
   "         COMPUTE   WK-032-YY            =      WK-032-YY    -   1   
   "         MOVE      12                   TO     WK-032-MM            
   "       ELSE                                                         
   "         COMPUTE   WK-032-MM            =      WK-032-MM    -   1.  
   "       MOVE        01                   TO     WK-032-DD.           
                                                                        
           MOVE        WK-032-YY            TO     WK-032-KEY-YY.       
           MOVE        WK-032-MM            TO     WK-032-KEY-MM.       
           MOVE        WK-032-DD            TO     WK-032-KEY-DD.       
880316**** MOVE        '0162106'            TO     WK-032-KEY-BRANCH.   
880316     MOVE        ZEROS                TO     WK-032-KEY-BRANCH.   
   "       MOVE        WK-032-KEY           TO     CMF02.               
           MOVE        14                   TO     W-LEN.               
           MOVE        SPACES               TO     CDC-RECORD-IN-G.     
           MOVE        SPACES               TO     CDC-ERROR-MSG.       
           MOVE        1                    TO     IDX2.                
           PERFORM     3902-MV-DATA-RTN    THRU    3902-EXIT            
           VARYING     IDX2 FROM  1  BY  1 UNTIL   IDX2 >  W-LEN.       
           PERFORM     4000-CVT-ITS-RTN     THRU   4000-EXIT.           
           PERFORM     3903-MV-DATA-RTN     THRU   3903-EXIT            
           VARYING     IDX2 FROM  1  BY  1  UNTIL  IDX2 >  W-LEN.       
           MOVE        CMF02                TO     WMF032-KEY.          
                                                                        
           EXEC  CICS  STARTBR                                          
                       DATASET('CBMF032')                               
                       RIDFLD(WMF032-KEY)                               
                       GTEQ                                             
           END-EXEC.                                                    
                                                                        
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF032')                               
                       INTO(TPSMF032)                                   
                       LENGTH(MF032-LENGTH)                             
                       RIDFLD(WMF032-KEY)                               
           END-EXEC.                                                    
                                                                        
       3400-READNEXT-MF032-RTN.                                         
           MOVE        TPSMF032             TO    TTSMF032.             
           MOVE        TPSMF032             TO    CMF02.                
           MOVE        58                   TO    W-LEN.                
           MOVE        SPACES               TO    CDC-RECORD-IN-G.      
           MOVE        SPACES               TO    CDC-ERROR-MSG.        
           MOVE        1                    TO    IDX2.                 
           PERFORM     3902-MV-DATA-RTN     THRU  3902-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           PERFORM     4100-CVT-STI-RTN     THRU  4100-EXIT.            
           PERFORM     3903-MV-DATA-RTN     THRU  3903-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           MOVE        CMF02                TO    TIPMF032.             
880108*    DISPLAY     WK-032-DATE  UPON CONSOLE.                       
  "        MOVE        TIPMF032            TO      TP-032-DATE.         
  "   *    DISPLAY     'TP =' TP-032-DATE  UPON CONSOLE.                
  "        MOVE        TP-032-DATE         TO      WS-032-DATE.         
  "   *    DISPLAY     'WS =' WS-032-DATE  UPON CONSOLE.                
                                                                        
           IF          MF032-BRANCH-CODE    =     '9999999'             
           THEN                                                         
             GO   TO   3400-READNEXT-RTN.                               
      *    ENDIF                                                        
880108     IF          WS-032-MM    NOT    =       WK-032-MM            
  "        THEN                                                         
  "            GO      TO     3400-READNEXT-RTN.                        
                                                                        
           IF         (MF032-NONINT-AMT-MARK =    '-')                  
           THEN                                                         
             COMPUTE  WMF032-AMT            =     MF032-NONINT-AMT      
                                            *     (-1)                  
           ELSE                                                         
             COMPUTE  WMF032-AMT            =     MF032-NONINT-AMT      
                                            *     (1).                  
      *    ENDIF                                                        
                                                                        
           IF         (WMF032-FLAG          =     'ON')                 
                 AND  (MF032-DD        NOT  =     01)                   
           THEN                                                         
             MOVE     'MB56'              TO      MSG-P-OUT-CODE        
             PERFORM  917-ERR-MSG-OUT-RTN.                              
      *    ENDIF                                                        
           IF         (WMF032-FLAG1         =     'ON')                 
CCCC             AND  (MF032-DD             =     01)                   
CCCC       THEN                                                         
CCCC         PERFORM   2999-SEARCH-DATE-RTN THRU  2999-EXIT             
CCCC         MOVE      02                   TO    WK-CHK-DD             
CCCCC        MOVE      'OFF'                TO    WMF032-FLAG.          
CCCC  *    ELSE                                                         
CCCC  *      IF       (MF032-DD         NOT =     WK-CHK-DD)            
CCCC  *      THEN                                                       
CCCC  *        PERFORM 3412-SUM-MF032-RTN   THRU  3412-EXIT             
CCCC  *                UNTIL    (MF032-DD   =     WK-CHK-DD)            
CCCC  *        GO   TO 3400-READNEXT-RTN                                
CCCC  *      ELSE                                                       
CCCC  *        COMPUTE WK-CHK-DD            =     WK-CHK-DD             
CCCC  *                                     +     1.                    
      *    ENDIF                                                        
                                                                        
                                                                        
           MOVE        MF032-DD             TO    SMF032-DD.            
           MOVE        WMF032-AMT           TO    SMF032-NONINT-TOT.    
           COMPUTE     WMF032-NONINT-TOT    =     WMF032-NONINT-TOT     
                                            +     WMF032-AMT.           
  "   *    IF          WS-032-MM    NOT    =       WK-032-MM            
  "   *    THEN                                                         
  "   *        GO      TO     3411-CHECK-DATA-RTN.                      
       3400-READNEXT-RTN.                                               
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF032')                               
                       INTO(TPSMF032)                                   
                       LENGTH(MF032-LENGTH)                             
                       RIDFLD(WMF032-KEY)                               
           END-EXEC.                                                    
           GO    TO    3400-READNEXT-MF032-RTN.                         
       3411-CHECK-DATA-RTN.                                             
           MOVE        WKX-DD              TO      WK-CHK-DD.           
CCC   *    IF         (SMF032-DD        NOT =      WK-CHK-DD)           
CCC   *    THEN                                                         
CCC   *      PERFORM   3412-SUM-MF032-RTN   THRU   3412-EXIT            
CCC   *                UNTIL    (SMF032-DD  =      WK-CHK-DD).          
CCC   *    ENDIF                                                        
           COMPUTE     WMF032-TOT  ROUNDED =     ((WMF032-NONINT-TOT    
                                           /       WKX-DD)              
                                           *       0.6)                 
                                           /       1000.                
                                                                        
           COMPUTE     WMF032-TOTAL        =       WMF032-TOT           
                                           *       1000.                
                                                                        
           PERFORM     3402-UPDATE-MF032-RTN                            
                                            THRU   3402-EXIT.           
       3400-EXIT.                                                       
           EXIT.                                                        
       3402-UPDATE-MF032-RTN.                                           
           EXEC CICS   HANDLE CONDITION                                 
                       NOTFND(3402-WRITE-MF032-RTN)                     
                       END-EXEC.                                        
                                                                        
           MOVE        WKS-DATA-DATE        TO     WMF032-DATA-DATE.    
           MOVE        '9999999'            TO     WMF032-BRANCH-CODE.  
           MOVE        0                    TO     WMF032-DD.           
           MOVE        WMF032-KEY           TO     CMF02.               
           MOVE        14                   TO     W-LEN.               
           MOVE        SPACES               TO     CDC-RECORD-IN-G.     
           MOVE        SPACES               TO     CDC-ERROR-MSG.       
           MOVE        1                    TO     IDX2.                
           PERFORM     3902-MV-DATA-RTN    THRU    3902-EXIT            
           VARYING     IDX2 FROM  1  BY  1 UNTIL   IDX2 >  W-LEN.       
           PERFORM     4000-CVT-ITS-RTN     THRU   4000-EXIT.           
           PERFORM     3903-MV-DATA-RTN     THRU   3903-EXIT            
           VARYING     IDX2 FROM  1  BY  1  UNTIL  IDX2 >  W-LEN.       
           MOVE        CMF02                TO     RMF032-KEY.          
                                                                        
           EXEC CICS   READ                                             
                       DATASET('CBMF032')                               
                       INTO(TMF032)                                     
                       RIDFLD(RMF032-KEY)                               
                       LENGTH(MF032-LENGTH)                             
                       UPDATE                                           
           END-EXEC.                                                    
                                                                        
           MOVE        TMF032               TO    CMF02.                
           MOVE        58                   TO    W-LEN.                
           MOVE        SPACES               TO    CDC-RECORD-IN-G.      
           MOVE        SPACES               TO    CDC-ERROR-MSG.        
           MOVE        1                    TO    IDX2.                 
           PERFORM     3902-MV-DATA-RTN     THRU  3902-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           PERFORM     4100-CVT-STI-RTN     THRU  4100-EXIT.            
           PERFORM     3903-MV-DATA-RTN     THRU  3903-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           MOVE        CMF02                TO    WMF032.               
                                                                        
880303     MOVE      ZEROS                TO    WMF032-NONINT-NO.       
880303     MOVE      ZEROS                TO    WMF032-INT-NO.          
           IF          WMF032-TOTAL         <     0                     
           THEN                                                         
             MOVE      '-'                  TO    WMF032-NONINT-AMT-MARK
             MOVE      WMF032-TOTAL         TO    WMF032-NONINT-AMT     
           ELSE                                                         
             MOVE      '0'                  TO    WMF032-NONINT-AMT-MARK
             MOVE      WMF032-TOTAL         TO    WMF032-NONINT-AMT.    
      *    ENDIF                                                        
                                                                        
           MOVE        CTF-BIF-BAL          TO    CTF-BIF-BAL-G.        
           IF          CTF-BIF-BAL-M        =     '-'                   
           THEN                                                         
             COMPUTE   WS-BIF-BAL           =     WCTF-BIF-BAL          
                                            *     (-1)                  
           ELSE                                                         
             COMPUTE   WS-BIF-BAL           =     WCTF-BIF-BAL          
                                            *     (1).                  
      *    ENDIF                                                        
                                                                        
           COMPUTE     WMF032-REBACK-AMT    =     WS-BIF-BAL            
                                            -     WMF032-TOTAL.         
                                                                        
           IF          WMF032-REBACK-AMT    <     0                     
           THEN                                                         
             MOVE      '-'                  TO    WMF032-INT-AMT-MARK   
             MOVE      WMF032-REBACK-AMT    TO    WMF032-INT-AMT        
           ELSE                                                         
             MOVE      '0'                  TO    WMF032-INT-AMT-MARK   
             MOVE      WMF032-REBACK-AMT    TO    WMF032-INT-AMT.       
      *    ENDIF                                                        
                                                                        
      *    DISPLAY 'MARK= '  WMF032-INT-AMT-MARK  UPON CONSOLE.         
           MOVE        WMF032               TO    CMF02.                
           MOVE        58                   TO    W-LEN.                
           MOVE        SPACES               TO    CDC-RECORD-IN-G.      
           MOVE        SPACES               TO    CDC-ERROR-MSG.        
           MOVE        1                    TO    IDX2.                 
           PERFORM     3902-MV-DATA-RTN     THRU  3902-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           PERFORM     4000-CVT-ITS-RTN     THRU  4000-EXIT.            
           PERFORM     3903-MV-DATA-RTN     THRU  3903-EXIT             
           VARYING     IDX2 FROM  1  BY  1  UNTIL IDX2 >  W-LEN.        
           MOVE        CMF02                TO    TIPMF032.             
                                                                        
           EXEC  CICS  REWRITE  DATASET ('CBMF032')                     
                       FROM (TIPMF032)                                  
                       LENGTH(MF032-LENGTH)                             
           END-EXEC.                                                    
                                                                        
           GO    TO    3402-EXIT.                                       
       3402-WRITE-MF032-RTN.                                            
           MOVE        WKS-DATA-DATE        TO     WMF032-DATA-DATE.    
           MOVE        '9999999'            TO     WMF032-BRANCH-CODE.  
           MOVE        0                    TO     WMF032-DD.           
           IF          WMF032-TOTAL         <     0                     
           THEN                                                         
             MOVE      '-'                  TO    WMF032-NONINT-AMT-MARK
             MOVE      WMF032-TOTAL         TO    WMF032-NONINT-AMT     
           ELSE                                                         
             MOVE      '0'                  TO    WMF032-NONINT-AMT-MARK
             MOVE      WMF032-TOTAL         TO    WMF032-NONINT-AMT.    
      *    ENDIF                                                        
880303       MOVE      ZEROS                TO    WMF032-NONINT-NO.     
880303       MOVE      ZEROS                TO    WMF032-INT-NO.        
                                                                        
           MOVE        CTF-BIF-BAL          TO    CTF-BIF-BAL-G.        
           IF          CTF-BIF-BAL-M        =     '-'                   
           THEN                                                         
             COMPUTE   WS-BIF-BAL           =     WCTF-BIF-BAL          
                                            *     (-1)                  
           ELSE                                                         
             COMPUTE   WS-BIF-BAL           =     WCTF-BIF-BAL          
                                            *     (1).                  
      *    ENDIF                                                        
                                                                        
           COMPUTE     WMF032-REBACK-AMT    =      WS-BIF-BAL           
                                            -      WMF032-TOTAL.        
                                                                        
           IF          WMF032-REBACK-AMT    <      0                    
           THEN                                                         
             MOVE      '-'                  TO     WMF032-INT-AMT-MARK  
             COMPUTE   WMF032-INT-AMT       =      WMF032-REBACK-AMT    
                                            *      (-1)                 
           ELSE                                                         
             MOVE      '0'                  TO     WMF032-INT-AMT-MARK  
             MOVE      WMF032-REBACK-AMT    TO     WMF032-INT-AMT.      
      *    ENDIF                                                        
                                                                        
      *    DISPLAY 'INT = '  WMF032-INT-AMT    UPON CONSOLE.            
           MOVE        WMF032               TO     CMF02.               
           MOVE        58                   TO     W-LEN.               
           MOVE        SPACES               TO     CDC-RECORD-IN-G.     
           MOVE        SPACES               TO     CDC-ERROR-MSG.       
           MOVE        1                    TO     IDX2.                
           PERFORM     3902-MV-DATA-RTN     THRU   3902-EXIT            
           VARYING     IDX2 FROM  1  BY  1  UNTIL  IDX2 >  W-LEN.       
           PERFORM     4000-CVT-ITS-RTN     THRU   4000-EXIT.           
           PERFORM     3903-MV-DATA-RTN     THRU   3903-EXIT            
           VARYING     IDX2 FROM  1  BY  1  UNTIL  IDX2 >  W-LEN.       
           MOVE        CMF02                TO     TIPMF032.            
                                                                        
           EXEC     CICS     WRITE    DATASET('CBMF032')                
                                      FROM(TIPMF032)                    
                                      LENGTH(MF032-LENGTH)              
                                      RIDFLD(MF032-KEY)                 
           END-EXEC.                                                    
                                                                        
       3402-EXIT.                                                       
           EXIT.                                                        
      *3412-SUM-MF032-RTN.                                              
      *    MOVE        TTSMF032             TO    WMF032.               
**                                                                      
      *    EXEC     CICS     WRITE    DATASET('CBMF032')                
      *                               FROM(TIPMF032)                    
      *                               LENGTH(MF032-LENGTH)              
      *                               RIDFLD(MF032-KEY)                 
      *    END-EXEC.                                                    
**                                                                      
      *    ADD         SMF032-NONINT-TOT    TO    WMF032-NONINT-TOT.    
      *    COMPUTE     WK-CHK-DD            =     WK-CHK-DD             
      *                                     +     1.                    
      *3412-EXIT.                                                       
      *    EXIT.                                                        
       3500-MF033-RETRIV-RTN.                                           
           EXEC CICS   HANDLE CONDITION                                 
                       ENDFILE(3500-EXIT)                               
                       ERROR(3500-EXIT)                                 
                       END-EXEC.                                        
           MOVE        SPACES   TO  MF033-KEY.                          
           EXEC  CICS  STARTBR                                          
                       DATASET('CBMF033')                               
                       RIDFLD(MF033-KEY)                                
           END-EXEC.                                                    
       3500-MF033-READNEXT-RTN.                                         
                                                                        
           EXEC  CICS  READNEXT                                         
                       DATASET('CBMF033')                               
                       INTO(TIPMF033)                                   
                       LENGTH(MF033-LENGTH)                             
                       RIDFLD(MF033-KEY)                                
           END-EXEC.                                                    
            IF         MF033-BRANCH-CODE   =  9999999                   
            THEN                                                        
                       GO   TO    3500-MF033-READNEXT-RTN.              
      *    ENDIF                                                        
            ADD        1           TO   WMF033-DATA-CNT.                
            GO         TO    3500-MF033-READNEXT-RTN.                   
950327 3500-EXIT.                                                       
           EXIT.                                                        
       3510-MF033-RETRIV-RTN.                                           
                                                                        
           EXEC CICS   HANDLE CONDITION                                 
                       NOTFND(3510-9999-WRITE-RTN)                      
                       END-EXEC.                                        
981125     MOVE        TWA-TXN-YY7         TO   WKS-END-YY.             
           MOVE        TWA-TXN-MM7         TO   WKS-END-MM.             
           MOVE        TWA-TXN-DD7         TO   WKS-END-DD.             
           IF          WKS-END-DD  NOT  <  1                            
               AND     WKS-END-DD       <  11                           
           THEN                                                         
                 MOVE  3               TO       WMF033-DATE-SECTION     
                 IF    WKS-END-MM      =  01                            
                 THEN                                                   
                       COMPUTE   WKS-END-YY  = WKS-END-YY - 1           
                       MOVE      12          TO WKS-END-MM              
                 ELSE                                                   
                       COMPUTE   WKS-END-MM  = WKS-END-MM - 1.          
      *    ENDIF                                                        
           IF          WKS-END-DD  NOT  <  11                           
               AND     WKS-END-DD       <  21                           
           THEN                                                         
                 MOVE  1               TO       WMF033-DATE-SECTION.    
      *    ENDIF                                                        
           IF          WKS-END-DD  NOT  <  21                           
               AND     WKS-END-DD       <  31                           
           THEN                                                         
                 MOVE  2               TO       WMF033-DATE-SECTION.    
      *    ENDIF                                                        
                                                                        
           MOVE        SPACES               TO     WMF033-ACCOUNT-NO.   
           MOVE        WKS-END-DATE         TO     WKS-TXN-DATE.        
           MOVE        WKS-DATA-DATE        TO     WMF033-DATA-DATE.    
           MOVE        9999999              TO     WMF033-BRANCH-CODE.  
           MOVE        ZEROS                TO     WMF033-OPEN-ACC      
                                                   WMF033-APRV-NO       
                                                   WMF033-CHAR-CODE     
                                                   WMF033-INT-CODE      
                                                   WMF033-CT-CODE       
                                                   WMF033-CURRENCY-CODE 
                                                   WMF033-TXN-CODE.     
           MOVE        SPACES               TO     WMF033-NAME.         
           MOVE        WMF033-DATA-CNT      TO     WMF033-ORG.          
***********MOVE        WMF033               TO     CMF033.              
***********MOVE        140                  TO     W-LEN.               
***********MOVE        SPACES               TO     CDC-RECORD-IN-G.     
*********  MOVE        SPACES               TO     CDC-ERROR-MSG.       
*********  MOVE        1                    TO     IDX2.                
********** PERFORM     39033-MV-DATA-RTN    THRU    39033-EXIT          
********** VARYING     IDX2 FROM  1  BY  1 UNTIL   IDX2 >  W-LEN.       
***********PERFORM     4000-CVT-ITS-RTN     THRU   4000-EXIT.           
*********  PERFORM     39034-MV-DATA-RTN     THRU   39034-EXIT          
*********  VARYING     IDX2 FROM  1  BY  1  UNTIL  IDX2 >  W-LEN.       
********** MOVE        CMF033               TO     RMF033.              
           EXEC  CICS  READ                                             
                 DATASET('CBMF033')                                     
                 INTO(WMF033)                                           
                 LENGTH(MF033-LENGTH)                                   
                 RIDFLD(WMF033-KEY)                                     
                 UPDATE                                                 
           END-EXEC.                                                    
           GO    TO  3510-9999-REW-RTN.                                 
       3510-9999-WRITE-RTN.                                             
           MOVE        WMF033-DATA-CNT      TO     WMF033-ORG.          
           EXEC  CICS  WRITE                                            
                       DATASET('CBMF033')                               
                       FROM(WMF033)                                     
                       LENGTH(MF033-LENGTH)                             
                       RIDFLD(WMF033-KEY)                               
           END-EXEC.                                                    
           GO          TO      3510-EXIT.                               
       3510-9999-REW-RTN.                                               
           MOVE        WMF033-DATA-CNT      TO     WMF033-ORG.          
           EXEC  CICS  REWRITE                                          
                       DATASET('CBMF033')                               
                       FROM(WMF033)                                     
           END-EXEC.                                                    
           EXEC        CICS    UNLOCK                                   
                       DATASET('CBMF033')                               
           END-EXEC.                                                    
                                                                        
           GO          TO      3510-EXIT.                               
950327 3510-EXIT.                                                       
           EXIT.                                                        
       3902-MV-DATA-RTN.                                                
           MOVE        CMF02-DATA(IDX2)    TO      CDC-RECORD-IN(IDX2). 
       3902-EXIT.                                                       
           EXIT.                                                        
       3903-MV-DATA-RTN.                                                
           MOVE        CDC-RECORD-IN(IDX2) TO      CMF02-DATA(IDX2).    
       3903-EXIT.                                                       
           EXIT.                                                        
       3904-MV-DATA-RTN.                                                
           MOVE        CMF01-DATA(IDX3)    TO      CDC-RECORD-IN(IDX3). 
       3904-EXIT.                                                       
           EXIT.                                                        
       3905-MV-DATA-RTN.                                                
           MOVE        CDC-RECORD-IN(IDX3) TO      CMF01-DATA(IDX3).    
       3905-EXIT.                                                       
           EXIT.                                                        
950327 39033-MV-DATA-RTN.                                               
           MOVE        CMF033-DATA(IDX2)   TO      CDC-RECORD-IN(IDX2). 
       39033-EXIT.                                                      
           EXIT.                                                        
950327 39034-MV-DATA-RTN.                                               
           MOVE        CDC-RECORD-IN(IDX2) TO      CMF033-DATA(IDX2).   
       39034-EXIT.                                                      
           EXIT.                                                        
       4000-CVT-ITS-RTN.                                                
      *-------------------------------------------------------          
           MOVE        '//4000-CVT//'      TO      WK-PARA.             
           MOVE        'ITS'               TO      CDC-CVT-TYPE.        
           MOVE        W-LEN               TO      WK-REC-LEN.          
           MOVE        WK-REC-LEN          TO      CDC-IN-LEN.          
           MOVE        WK-REC-LEN          TO      CDC-MAX-LEN.         
           IF          CDC-C-ITS                                        
           THEN                                                         
             MOVE      CDC-M-ITS-SPECIAL-SYMBOL                         
                                           TO      CDC-SPECIAL-SYMBOL   
           ELSE                                                         
             MOVE      CDC-M-STI-SPECIAL-SYMBOL                         
                                           TO      CDC-SPECIAL-SYMBOL.  
      *    ENDIF                                                        
           MOVE        CDC-M-CHINESE-NO    TO      CDC-CHINESE-FLAG.    
      *    MOVE        CDC-P-BASIC-DATA-1  TO      CDC-RECORD-IN-G.     
           PERFORM     9000-CALL-CCDCMAIN-RTN                           
                                           THRU    9000-EXIT.           
      *    MOVE        CDC-RECORD-IN-G     TO      CDC-P-BASIC-DATA-1.  
       4000-EXIT.                                                       
           EXIT.                                                        
       4100-CVT-STI-RTN.                                                
      *-------------------------------------------------------          
           MOVE        '//4000-CVT//'      TO      WK-PARA.             
           MOVE        'STI'               TO      CDC-CVT-TYPE.        
           MOVE        W-LEN               TO      WK-REC-LEN.          
           MOVE        WK-REC-LEN          TO      CDC-IN-LEN.          
           MOVE        WK-REC-LEN          TO      CDC-MAX-LEN.         
           IF          CDC-C-ITS                                        
           THEN                                                         
             MOVE      CDC-M-ITS-SPECIAL-SYMBOL                         
                                           TO      CDC-SPECIAL-SYMBOL   
           ELSE                                                         
             MOVE      CDC-M-STI-SPECIAL-SYMBOL                         
                                           TO      CDC-SPECIAL-SYMBOL.  
      *    ENDIF                                                        
           MOVE        CDC-M-CHINESE-NO    TO      CDC-CHINESE-FLAG.    
      *    MOVE        CDC-P-BASIC-DATA-1  TO      CDC-RECORD-IN-G.     
           PERFORM     9000-CALL-CCDCMAIN-RTN                           
                                           THRU    9000-EXIT.           
      *    MOVE        CDC-RECORD-IN-G     TO      CDC-P-BASIC-DATA-1.  
       4100-EXIT.                                                       
           EXIT.                                                        
       5000-FILE-REPAIR-RTN.                                            
           MOVE        2                   TO      WK-SYSTEM-ID.        
           MOVE        'CBCCT01'           TO      WK-COMPRESS-ID.      
                                                                        
           IF          CTF-INQ-TYPE        =       1                    
           THEN                                                         
             MOVE      'TIPMF01 '          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       2                    
           THEN                                                         
             MOVE      'TIPMF02 '          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       3                    
           THEN                                                         
************ MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF031'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       4                    
           THEN                                                         
             MOVE      'TIPMF032'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       5                    
           THEN                                                         
             MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF051'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
990525     IF          CTF-INQ-TYPE        =       0                    
           THEN                                                         
             MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF034'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
           IF          CTF-INQ-TYPE        =       6                    
           THEN                                                         
             MOVE      'TIPMF052'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
990525     IF          CTF-INQ-TYPE        =       9                    
           THEN                                                         
             MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF035'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
950327     IF          CTF-INQ-TYPE        =       7                    
           THEN                                                         
             MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF033'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
A91218     IF          CTF-INQ-TYPE        =       8                    
           THEN                                                         
             MOVE      '       '           TO      WK-COMPRESS-ID       
             MOVE      'TIPMF20 '          TO      WK-FILE-ID.          
      *    ENDIF                                                        
950801*    IF          CTF-INQ-TYPE        =       8                    
  "   *    THEN                                                         
  "   *      MOVE      '       '           TO      WK-COMPRESS-ID       
  "   *      MOVE      'TIPMF061'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
890306*    IF          CTF-INQ-TYPE        =       8                    
  "   *    THEN                                                         
  "   *      MOVE      'TIPMF062'          TO      WK-FILE-ID.          
  "   *    ENDIF                                                        
                                                                        
890306*    IF          CTF-INQ-TYPE        =       9                    
 "    *    THEN                                                         
 "    *      MOVE      'TIPMF063'          TO      WK-FILE-ID.          
      *    ENDIF                                                        
                                                                        
            EXEC     CICS   START        TRANSID ('IFTO')               
                                         TERMID ('CNSL')                
                                         INTERVAL(WK-IS-TIME)           
                                         FROM    (WK-IFTS-DATA)         
                                         LENGTH  (WK-LEN)               
            END-EXEC.                                                   

       5000-EXIT.                                                       
           EXIT.
      ***
       7000-MSG-OUTPUT-RTN.
           MOVE        '7000-MSG'          TO      WK-PARA.
           MOVE        'T700'              TO      MSG-P-OUT-CODE.
           MOVE        44                  TO      MSG-P-LENGTH.
           MOVE        MSG-M-DISPLAY       TO      MSG-P-OUT-TM-TYPE.
           MOVE        '0000'              TO      T034-RESPONSE-CODE.  
           MOVE        '稍後請啟動交易結果查詢'                       
                                           TO      T034-CONTENT.        

      **--- OUTPUT -------------------------------------------------**
           PERFORM     904-MSG-OUTPUT-RTN  THRU    904-EXIT.
           PERFORM     7001-HARDCOPY-RTN   THRU    7001-EXIT.

       7000-EXIT.
           EXIT.
      ***
       7001-HARDCOPY-RTN.
           MOVE        TWA-IS-STAN-NO      TO      L86P-STAN-NO.
           MOVE        TWA-BR-CODE         TO      L86P-TWA-BR-CODE.
           MOVE        TWA-TXN-ID-CODE     TO      L86P-TXN-ID.
           EXEC        CICS    START   TRANSID  ('OPCT')
                                       INTERVAL (0)
                                       FROM     (L86P-DATA)
                                       LENGTH   (38)
                                       TERMID   (CWHCTID)
                                       END-EXEC.

       7001-EXIT.
           EXIT.
       8000-TXN-END-RTN.
           MOVE        '/8000-TXN/'        TO      WK-PARA.
            EXEC CICS RETURN END-EXEC.                                  
       8000-EXIT.
           EXIT.
      **------------------------------------------------------------**
      *
       901-TXN-INIT-RTN.                   COPY    CRK01.
      *
       904-MSG-OUTPUT-RTN.                 COPY    CBK04.
      *
       961-CHK-VSAM-RTRN-CODE-0.           COPY    CRK61.
      *
       965-MSG-ERR-HNDL-RTN.               COPY    CRK65.
      *
       9146-LN-ERR-MSG-OUT-RTN.            COPY    LNK146.
       9146-END-EXIT.
           EXIT.
      *
       9147-LN-UCP-WRITE-ERR-HDL-RTN.      COPY    LNK147.
       9147-END-EXIT.
           EXIT.
      *
       917-ERR-MSG-OUT-RTN.                COPY      PPK17.
           EJECT
       935-UCPWRITE-HDL-RTN.               COPY      PPK35.
           EJECT
       601-FILE-NOT-OPEN.
           MOVE      'MC55'              TO      MSG-P-OUT-CODE.
           PERFORM    917-ERR-MSG-OUT-RTN.
       602-NO-REC-FOUND.
           MOVE      'MC51'              TO      MSG-P-OUT-CODE.
           PERFORM    917-ERR-MSG-OUT-RTN.
       603-OTHER-ERROR.
           MOVE      'MC5A'              TO      MSG-P-OUT-CODE.
           PERFORM    917-ERR-MSG-OUT-RTN.
       604-END-OF-REC.
           GO   TO    5000-EXIT.                                        
       605-DUPREC-RTN.                                                  
           MOVE      'M5BC'              TO      MSG-P-OUT-CODE.        
           PERFORM    917-ERR-MSG-OUT-RTN.                              
       9164-LN-DATE-ERR-HDL-RTN.           COPY    LNK164.              
       9164-END-EXIT.                                                   
           EXIT.                                                        
      *                                                                 
       9165-INT-SUB-ERR-HDL-RTN.           COPY    LNK165.              
       9165-END-EXIT.                                                   
           EXIT.                                                        
      *                                                                 
       9171-LN-INT-AREA-CLEAR.             COPY    LNK171.              
       9171-END-EXIT.                                                   
           EXIT.                                                        
       9000-CALL-CCDCMAIN-RTN.                                          
           MOVE        '//9000-CAL//'      TO      WK-PARA.             
           MOVE        ZEROS               TO      CDC-OUT-LEN.         
8601M *    COMPUTE     WK-CDC-LEN-G        =       WK-REC-LEN + 109.    
           COMPUTE     WK-CDC-LEN-G        =       CDC-MAX-LEN + 109.   
090421**** EXEC        CICS    LINK        PROGRAM('CCDCMAIN')          
090421     EXEC        CICS    LINK        PROGRAM('CVMCMAIN')          
                                           COMMAREA(CDC)                
                                           LENGTH(WK-CDC-LEN)           
           END-EXEC.                                                    
           IF          CDC-ERROR-MSG-CLASS =  'W'                       
                   OR  CDC-ERROR-MSG-CLASS =  'E'                       
           THEN                                                         
             PERFORM     9200-DISP-CNSL-RTN  THRU    9200-EXIT.         
      *    ENDIF                                                        
       9000-EXIT.                                                       
           EXIT.                                                        
      *                                                                 
       9200-DISP-CNSL-RTN.                                              
           MOVE  CDC-ERROR-MSG-ID           TO   OC-CTO-ERR-MSG.        
           MOVE  CDC-CVT-TYPE               TO   OC-CTO-CVT-TYPE.       
           IF    CDC-C-ITS                                              
           THEN                                                         
             MOVE  SV-SOURCE-BANK           TO   OC-CTO-SOURCE-BANK     
             MOVE  SV-STAN-NO               TO   OC-CTO-STAN-NO         
             MOVE  SV-MSG-TYPE              TO   OC-CTO-MSG-TYPE        
             MOVE  SV-PROCESS-CODE          TO   OC-CTO-PROCESS-CODE    
           ELSE                                                         
             MOVE  CDC-P-SOURCE-BR          TO   OC-CTO-SOURCE-BANK     
             MOVE  CDC-P-STAN-NO            TO   OC-CTO-STAN-NO         
             MOVE  CDC-P-MSG-TYPE           TO   OC-CTO-MSG-TYPE        
             MOVE  CDC-P-PROCESS-CODE       TO   OC-CTO-PROCESS-CODE.   
           EXEC CICS START TRANSID ('OPCT')                             
                           INTERVAL(0)                                  
                           FROM    (CONSOLE-DATA)                       
                           LENGTH  (69)                                 
                           TERMID   ('CNSL')                            
                           END-EXEC.                                    
       9200-EXIT.                                                       
           EXIT.                                                        
      **------------------------------------------------------------**  
       9999-ERR-MSG-OUT-RTN.                                            
           MOVE        'T700'              TO      MSG-P-OUT-CODE.
           MOVE        44                  TO      MSG-P-LENGTH.
           MOVE        MSG-M-DISPLAY       TO      MSG-P-OUT-TM-TYPE.
           MOVE        WK-MSG-CODE         TO      T034-RESPONSE-CODE.  
           MOVE        WK-MSG-CONTENT      TO      T034-CONTENT.        

      **--- OUTPUT -------------------------------------------------**
           PERFORM     904-MSG-OUTPUT-RTN  THRU    904-EXIT.
      **------------------------------------------------------------**
           PERFORM     8000-TXN-END-RTN    THRU    8000-EXIT.
       9999-EXIT.
           EXIT.
       TEST-RTN.                                                        
           EXEC CICS RETURN END-EXEC.
