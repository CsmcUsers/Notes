       ID DIVISION.
       PROGRAM-ID.     LNL671.                                          
      ******************************************************************
      * COMPILE---CCC        LIB---126                                  
      *-----------------------------------------------------------------
      *交易：6710現金收回本息                                       
      *      6722現金通知傳票記帳(6720--LNL672啟動)                2
      *        6740混合收回本息                                       
      *      6752混合通知傳票記帳(6750--LNL672啟動)                2
      *        6770轉帳收回本息                                       
      *      6782轉帳通知傳票記帳(6780--LNL672啟動)                2
      *      6783學貸自動銷帳記帳(6630啟動)                        7
      *-----------------------------------------------------------------
      *【AAAA】重要設定／【TEST】檢核顯示指令－移到LNK671C        
      *※程式異動說明請參閱【LNK671C】                              
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
------*-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *---------------固定值工作區                                    
       01  FIXED-DATA-WORK-AREA.                                        
060421     05  COMP-LENGTH             PIC S9(04) COMP VALUE ZEROS.     
891019     05  RATE-LEN                PIC S9(04) COMP VALUE +120.      
           05  APS-TIF-LENGTH          PIC S9(04) COMP VALUE +21.       
991203     05  COST-LENGTH             PIC S9(04) COMP VALUE +100.      
"          05  ACIF-LEN                PIC S9(04) COMP VALUE +2307.     
"          05  TEMP-FIX-FREE           PIC  9(11)    VALUE ZEROS.       
"          05  TEMP-FIX-COST           PIC  9(11)    VALUE ZEROS.       
991203     05  SAVE-ADJ-AMT            PIC  S9(11)    VALUE ZEROS.      
991203     05  WK-ADJ-FLAG             PIC  X         VALUE SPACES.     
980506     05  INT-LENGTH              PIC S9(04) COMP VALUE +600.      
           05  BIF-LENGTH              PIC S9(04) COMP VALUE +176.      
990118     05  TIF-W-LENG              PIC S9(04) COMP VALUE +200.      
           05  TOT-APRV-LEN            PIC S9(04) COMP VALUE +100.      
           05  PB-H-LEN                PIC S9(04) COMP VALUE +250.      
           05  CT-WMCHK-LEN            PIC S9(04) COMP VALUE +59.       
901109     05  LBI-LENGTH              PIC S9(04) COMP VALUE +150.      
930730     05  CLS-LENGTH              PIC S9(04) COMP VALUE +120.      
950912     05  ACLNID-LEN              PIC S9(04) COMP VALUE +60.       
120710     05  SCHSUDY-LEN             PIC S9(04) COMP VALUE +700.      
961226     05  ED04-LEN                PIC S9(04) COMP VALUE +350.      
981125     05  F24-LENGTH              PIC S9(04) COMP VALUE +250.      
A10423     05  F28-LENGTH              PIC S9(04) COMP VALUE +200.      
981223     05  FAX-LENGTH              PIC S9(04) COMP VALUE +140.      
000218     05  MJIBD-LEN               PIC S9(4)  COMP VALUE +100.      
010130     05  RLTM-LENGTH             PIC S9(04) COMP VALUE +128.      
131202     05  CFBLACK-LENGTH          PIC S9(04) COMP VALUE +200.      
030224     05  SHATM-LEN               PIC S9(4) COMP VALUE +200.       
060824     05  BAD69-LENGTH            PIC S9(4) COMP VALUE +180.       
B00309     05  MJIB5-LEN               PIC S9(4) COMP VALUE +200.       
990118 01  TIF-W-KEY.                                                   
"        05  TIF-W-BR-CODE                 PIC 9(3).                    
990118   05  TIF-W-TELLER-CODE             PIC 99.                      
050122*---------------ＣＴＦ暫存區                                    
AAAA   01  WCTF-AREA                        PIC X(500) VALUE SPACES.    
040317*---------------日數計算模組暫存工作區                          
"      01  FILLER  PIC X(16) VALUE '//  DAT-P-WK  //'.                  
"      01  DAT-P-WORK-AREA.                                             
"          05  WK-DAT-BEG-DATE              PIC 9(07) VALUE ZEROS.      
"          05  WK-DAT-END-DATE              PIC 9(07) VALUE ZEROS.      
"          05  WK-DAT-DAYS                  PIC 9(04) VALUE ZEROS.      
"          05  WK-DAT-TERM-UNIT             PIC 9(02) VALUE ZEROS.      
"      01  WK-YEAR                          PIC 9(04) VALUE ZEROS.      
"      01  WK-M-I                           PIC 9(02) VALUE ZEROS.      
"      01  WK-M-J                           PIC 9(02) VALUE ZEROS.      
"      01  WS-REMAINER                      PIC 9(01) VALUE ZEROS.      
040317*---------------一般工作區                                      
TTTT   01  LN-SHOW-AMT                     PIC 9(10) VALUE ZEROS.       
"      01  FILLER            PIC X(16) VALUE '//  BASIC-WK  //'.        
"      01  BASIC-WORK-AREA.                                             
050614     05  FILE-LENGTH                  PIC S9(04) COMP VALUE ZEROS.
           05  DISP-ERROR-END.                                          
               10  DISP-ERR-MARK1      PIC X(14) VALUE 'TXN-ERROR-END:'.
               10  DISP-TXN-ID-CODE         PIC X(05) VALUE SPACES.     
               10  FILLER                   PIC X(01) VALUE '/'.        
               10  DISP-PROG-NAME           PIC X(08) VALUE SPACES.     
               10  FILLER                   PIC X(01) VALUE '/'.        
               10  DISP-TM-ID-NO            PIC X(05) VALUE SPACES.     
               10  FILLER                   PIC X(01) VALUE '/'.        
               10  DISP-TXN-TIME            PIC X(06) VALUE SPACES.     
050614     05  DISP-MARK                    PIC X(01) VALUE SPACES.     
"          05  DISP1-G                      PIC X(70) VALUE SPACES.     
050614     05  FILLER             REDEFINES DISP1-G.                    
"              10  DISP1-HEAD               PIC X(13).                  
"              10  DISP1-AMT                PIC --,---,---,--9.99.      
"              10  FILLER                   PIC X(40).                  
050614     05  WK-WEST-DATE                 PIC 9(08) VALUE ZEROS.      
"          05  FILLER             REDEFINES WK-WEST-DATE.               
"              10  WK-WEST-YY               PIC 9(04).                  
"              10  WK-WEST-MM               PIC 9(02).                  
"              10  WK-WEST-DD               PIC 9(02).                  
           05  WK-WORK-DATE            PIC 9(07) VALUE ZEROS.           
           05  FILLER        REDEFINES WK-WORK-DATE.                    
               10  WK-WORK-YY          PIC 9(03).                       
               10  WK-WORK-MM          PIC 9(02).                       
               10  WK-WORK-DD          PIC 9(02).                       
920527     05  WK-ADJ-CODE             PIC 9(02) VALUE ZEROS.           
000131     05  WK-LN-ID-NO.                                             
"              10  WK-APRV-NO          PIC X(11) VALUE SPACES.          
000131         10  WK-LN-SER-NO        PIC X(04) VALUE SPACES.          
000131     05  WK-COST-MARK            PIC X     VALUE SPACE.           
940517     05  WK-CTF-TXN-SER-NO       PIC 9(10) VALUE ZEROS.           
960521     05  WK-LNBAD-TYPE           PIC 9 VALUE 0.                   
970929     05  WK-WORK-AMT             PIC S9(13)V99 VALUE ZEROS.       
971222     05  CHK-MARK1               PIC X(01) VALUE SPACES.          
980720     05  WK-ID-NO.                                                
"              10  WK-TAX-FIL          PIC X(02) VALUE ZEROS.           
980720         10  WK-TAX-ID           PIC X(08) VALUE ZEROS.           
981125     05  F24-MARK                PIC X(01) VALUE 'N'.             
000225     05  F-101-MARK              PIC X(01) VALUE 'N'.             
A10423     05  F28-MARK                PIC X(01) VALUE 'N'.             
000324     05  WK-TEMP-AMT             PIC 9(11) VALUE ZEROS.           
040317     05  WKS-RTRN-TERM-NO        PIC 9(03) VALUE ZEROS.           
"          05  WKS-REAL-RTRN-CODE      PIC X(01) VALUE SPACES.          
"          05  WK-LNM-TXN-AMT               PIC S9(13)V99 VALUE ZEROS.  
050614     05  WK-PRECLS-WARNING.                                       
141124         10  FILLER                   PIC X(25) VALUE             
"                X'0E4D7B54F951FA53EE505157D163C059E55B8754F94DB4426B'. 
******************'本案係限制清償期間案件，'.                         
141124         10  FILLER                   PIC X(24) VALUE             
"                X'53EE505159E553EE4C9A4D94619950E950E467E15BD461D8'.   
******************'限制期限內申請抵押權塗銷'.                         
"              10  FILLER                   PIC X(17) VALUE             
141124           X'5FE44D564E4D505D5DAF539D51D943410F'.                 
******************'需另收取違約金。'.                                 
041211*---------------大額通貨工作區                                  
"      01  TXN-9200-WORK-AREA.                                          
"          05  WK-LNM-CREAT-DATE            PIC 9(07) VALUE ZEROS.      
"          05  WK-LNM-CFM-SER               PIC 9(02) VALUE ZEROS.      
"          05  WLNM-CUST-ID                 PIC X(10) VALUE SPACES.     
"          05  FILLER             REDEFINES WLNM-CUST-ID.               
"              10  WLNM-TAX-ID              PIC X(08).                  
"              10  FILLER                   PIC X(02).                  
971222*---------------檔案存取錯誤訊息                                
"      01  MSG-FILE-ACCESS.                                             
"          05  MSG-FILE-LINE1.                                          
060421         10  MSG-FILE-ERR-CODE        PIC X(05) VALUE SPACES.     
"              10  MSG-FILE-ERROR-RSN       PIC X(34) VALUE SPACES.     
"              10  FILLER                   PIC X(13) VALUE SPACES.     
"          05  MSG-FILE-LINE2.                                          
"              10  FILLER                   PIC X(04) VALUE SPACES.     
"              10  MSG-FILE-NAME            PIC X(08) VALUE SPACES.     
"              10  FILLER                   PIC X(40) VALUE SPACES.     
971222*---------------ＣＦＮＡＭＥ工作區                              
"      01  FILLER  PIC X(16) VALUE '//  CF-NAME   //'. COPY CFKCFMV.    
060824 01  FILLER  PIC X(16) VALUE '//  BAD69     //'. COPY LNKBD69.    
"      01  FILLER  PIC X(16) VALUE '//  CFNM-WORK //'.                  
"      01  CFNAME-WORK-AREA.                                            
"          05  CFM-LENGTH              PIC S9(04) COMP VALUE +350.      
"          05  WS-CFM-ID.                                               
"              10  WS-CFM-ID-A         PIC X(02) VALUE SPACES.          
"              10  WS-CFM-ID-B         PIC X(08) VALUE SPACES.          
"          05  WK-CUST-NAME            PIC X(40) VALUE SPACES.          
"          05  WK-CUST-NAME-R     REDEFINES WK-CUST-NAME.               
"              10  WK-NAME-0E          PIC X(01).                       
"              10  FILLER              PIC X(38).                       
"              10  WK-NAME-0F          PIC X(01).                       
001019*---------------ＡＧＤＩ檔工作區                                
"      01  FILLER  PIC X(16) VALUE '//   AGDI-WK  //'.                  
"      01  LNAGDIF-WORK-AREA.                                           
"          05  AGDI-LENGTH             PIC S9(04) COMP VALUE +200.      
"          05  WAGDI-DIFF-BAL               PIC 9(13) VALUE ZEROS.      
"          05  WAGDI-KEY                    PIC X(37) VALUE SPACES.     
"          05  FILLER             REDEFINES WAGDI-KEY.                  
"              10  WAGDI-ACC-NO             PIC X(12).                  
"              10  WAGDI-LOAN-ID            PIC X(25).                  
B00309 01  WS-AML-DATA.                                                 
           05 WS-AML-CODE                   PIC X(14) VALUE SPACES.     
           05 WS-TXN-CUST-NAME              PIC X(40) VALUE SPACES.     
           05 WS-BLACK-LIST                 PIC X(01) VALUE SPACES.     
      *-----------------------------------------------------------------
891011 77  WK-MSG-PB-MARK                  PIC X(60)                    
            VALUE  '請作１３０交易恢復撥款帳號ＡＴＭ提款收取手續費'.  
931216 77  WK-TXN-ID-CODE                  PIC 9(04) VALUE ZEROS.       
931216 77  WK-ACC-TXN-SEQ-NO               PIC 9(05) VALUE ZEROS.       
   "   77  WS-TELLER-CODE                  PIC 9(2) VALUE 0.
       77  WKS-RATE                        PIC 9(3)V9(10).              
891019 77  WKS-RATE2                       PIC S99V999 VALUE +0.        
891019 77  WKS-RATE3                       PIC S99V999 VALUE +0.        
910731 77  WKS-RATE4                       PIC S99V999 VALUE +0.        
891019 77  CHG-RATE-INDEX                  PIC 9      VALUE 0.          
891019 77  END-RATE-INDEX                  PIC 9      VALUE 0.          
920514 77  WS-BEF-47-INT                   PIC S99V999 VALUE +0.        
791114 77  WKS-DUE-DATE                    PIC 9(7)      VALUE  0.
891019 77  WKS-PRE-E-DATE                  PIC 9(7) VALUE 0.            
891019 77  WKS-END-RATE-DATE               PIC 9(7) VALUE 0.            
891019 77  WKS-END-TEMP-DATE               PIC 9(7) VALUE 0.            
891019 77  WKS-COMP-DATE                   PIC 9(7) VALUE 0.            
A10423 77  WKS-TERM-NO                     PIC 9(3) VALUE 0.            
791114 77  WKS-RECV-INT-DATE               PIC 9(7).
       77  WKS-NEXT-RTRN-DATE              PIC 9(7).                    
831013 77  WK-RECV-INT-DATE                PIC 9(7).
900821 77  WK-L-RECV-INT-DATE              PIC 9(7).                    
000628 77  SAVE-ADJ-CODE                   PIC X VALUE SPACE.           
       77  WK-DUE-DATE                     PIC 9(7).
       77  WS-PARAGRAPH                    PIC X(16)       VALUE SPACE.
890922 77  BEF-RTRN-STYLE                  PIC 9(1) VALUE ZEROS.        
       77  WKS-INT-RATE                    PIC 9(2)V9(3) VALUE  0.      
       77  WS-LNP-STOP-CODE                PIC 9(1).                    
       77  WS-INT                      PIC S9(11)V99 COMP-3 VALUE +0.   
       77  WS-INT-DELAY                PIC S9(11)V99 COMP-3 VALUE +0.   
       77  WS-INT-FINE                 PIC S9(11)V99 COMP-3 VALUE +0.   
       77  WS-NOR-DLY-DAYS             PIC 9(04) VALUE 0.               
       77  WS-DELAY-DAYS               PIC 9(04) VALUE 0.               
       77  WK-SAVE-DAYS                PIC 9(04) VALUE 0.               
       77  W-LNP-APRV-AMT                  PIC S9(11)V99 COMP-3.
       77  W-LNP-APRV-UNUSED-BAL           PIC S9(11)V9(2) COMP-3.
       77  W-LNL-LN-BAL                    PIC S9(11)V9(2) COMP-3.
       77  W-BAL                           PIC S9(11)V99   COMP-3.
       77  W-BAL-OTHER                     PIC S9(11)      COMP-3.      
       77  W-BAL-80                        PIC S9(11)V99   COMP-3.      
       77  W-CTF-AMT                       PIC S9(11)V9(2) COMP-3.
       77  CTF-W-LOG-AMT                   PIC S9(11)V9(2) COMP-3.
       77  W-LNP-FIN-PROP                  PIC 9(3)V99.
940824 77  SV-CUST-NAME                    PIC X(22) VALUE SPACE.       
990604 77  SW-TEMP-ACC-NO                  PIC 9(12).                   
990604   88  SW-921-ACC-NO              VALUE 240166102775 235166023901 
990604           101166346542  101166348480   101166348502 101166348529 
990604           101166349690  101166348545   101166400148 101166410356 
990604           101166413835  101166416940.                            
061206 77  WK-SIMP-CODE                    PIC X(01) VALUE SPACES.      
       77  SW-FINE-CHARGE                  PIC 9.
         88  SW-C-FINE-YES                 VALUE 0.
         88  SW-C-FINE-NO                  VALUE 1.
891019 77  WKS-CHG-SW                      PIC X.                       
         88  CHG-SW-Y                      VALUE 'Y'.                   
         88  CHG-SW-N                      VALUE 'N'.                   
       77  END-DATE-SW                     PIC 9.
         88  END-DATE-OFF                  VALUE 0.
         88  END-DATE-ON                   VALUE 1.
       77  FINE-SW                         PIC X(3) VALUE 'NO '.        
961226 77  W-AMT-10                        PIC 9(7) VALUE 0.            
961226 77  W-AMT-8                         PIC 9(7) VALUE 0.            
961226 77  W-AMT-2                         PIC 9(7) VALUE 0.            
961226 77  W-TYPE                          PIC 9(1) VALUE 0.            
080425 77  WK-9201-AMT             PIC  9(11)    VALUE  0.              
981223 01  WK-FAX-AMT                      PIC 9(11)    VALUE 0.        
960521 01  LNBAD-LENGTH                    PIC S9(04) COMP VALUE +700.  
050614*----------------國宅公教短墊款ＡＣＬＮＩＤ１                   
"      01  LN-ID1-REC.                                                  
"          05  LNID1-KEY                    PIC X(12) VALUE SPACES.     
"          05  LNID1-DATA                   PIC X(08) VALUE SPACES.     
060421 01  FILLER  PIC X(16) VALUE '//  BCVOT-WK  //'.    COPY BCKVOT.  
050614 01  FILLER  PIC X(16) VALUE '//  ACLNCRF   //'.    COPY ACKLNCRF.
891019 01  FILLER  PIC X(16) VALUE '//  LNKADRAT  //'.    COPY LNKADRAT.
891019 01  FILLER  PIC X(16) VALUE '//    CFKW    //'.    COPY CFKW.    
900105 01  FILLER  PIC X(16) VALUE '//  LNKCONTR  //'.    COPY LNKCONTR.
961226 01  FILLER  PIC X(16) VALUE '//  LNKED04   //'.    COPY LNKED04. 
981125 01  FILLER  PIC X(16) VALUE '//  ALNGVRF   //'.    COPY LNKF24.  
A10423 01  FILLER  PIC X(16) VALUE '//  LNCTNRF   //'.    COPY LNKF28.  
990118 01  FILLER  PIC X(16) VALUE '//  TIF-P     //'.    COPY PBKTIF.  
030224 01  FILLER  PIC X(16) VALUE '//   LNSHATM  //'.    COPY LNKATMF. 
"      01  WK-CHK-AMT                      PIC  9(13)     VALUE    0.   
"      01  I                               PIC  9(1)      VALUE    0.   
"      01  WK-CONTR-REL.                                                
"        05  WK-CONTR-DATA   OCCURS 4  TIMES.                           
"          10  WK-LNCNTR-ID-NO             PIC X(10).                   
"          10  WK-LNCNTR-ACC-NO            PIC 9(12).                   
"          10  WK-LNCNTR-NO                PIC 9(4).                    
"          10  WK-LNCNTR-NT-BAL            PIC 9(13).                   
"      01  SAVE-REL-NO                     PIC  9(4)      VALUE    0.   
"      01  SAVE-CUST-ID.                                                
"        05  SAVE-CUST-ID1                 PIC  X(2)      VALUE SPACES. 
"        05  SAVE-CUST-ID2                 PIC  X(8)      VALUE SPACES. 
960521 01  FILLER  PIC X(16) VALUE '//   LNBADF   //'. COPY LNKBADF.    
980513     COPY LNK671C.                                                
9807XX*LNK671C包含連動扣存摺存款工作區，就學貸款短期墊款檔案，　　    
9807XX*催收轉銷呆帳連動６４８交易　　　　　　　　　　　　　　　　     
981111*中途攤還７工作區                                               
      *------------ COPY   AP SYSTEM USE PARAMETER ---------------------
890426 01  FILLER  PIC X(16) VALUE '//  LNKALNBC  //'.  COPY LNKALNBC.  
       01  FILLER                          PIC X(8)  VALUE 'WSI DATA'.
860925 77  WK-WMCHK-CR-CASH-AMT            PIC S9(13)V99  VALUE ZEROS.  
"      01  FILLER  PIC X(16) VALUE '//  CTKMCHK   //'.  COPY CTKWMCHK.  
"      01  WK-WARNING-MSG.                                              
"        05  WK-WARNING-MARK               PIC X      VALUE  SPACE.     
"        05  WK-WARNING-MSG1               PIC X(40)  VALUE             
980316        '$$ ATTENTION, LN CR CASH >=   500,000 $$'.               
"        05  WK-WARNING-MSG2               PIC X(40)  VALUE             
"             '$$$ PLS   REGISTER CUSTOMER`S DATA   $$$'.               
"        05  WK-WARNING-MSG3               PIC X(40)  VALUE             
"             '                                        '.               
"        05  MSG-XXX1 PIC X(40) VALUE                                   
"                         '                $$$   $$$               '.   
"        05  MSG-XXX2 PIC X(40) VALUE                                   
"                         '            $$$$$$$$$$$$$$$$$           '.   
"        05  MSG-XXX3 PIC X(40) VALUE                                   
"                         '          $$$   $$$   $$$               '.   
"        05  MSG-XXX4 PIC X(40) VALUE                                   
"                         '            $$$$$$$$$$$$$$$$$           '.   
"        05  MSG-XXX5 PIC X(40) VALUE                                   
"                         '                $$$   $$$   $$$         '.   
"        05  MSG-XXX6 PIC X(40) VALUE                                   
"                         '            $$$$$$$$$$$$$$$$            '.   
"        05  MSG-XXX7 PIC X(40) VALUE                                   
"                         '                $$$   $$$               '.   
      *-----------------------------------------------------------------
040317 01  WLNP                             PIC X(400).                 
"      01  CLNP                             PIC X(400).                 
"      01  FILLER                 REDEFINES CLNP.                       
         05  CLNP-APRV-NO                   PIC 9(11).                  
         05  CLNP-DEP-CODE                  PIC 9(1).
         05  CLNP-CURRENCY-CODE             PIC 9(2).
         05  CLNP-ACC-CODE                  PIC 9(2).
         05  CLNP-APRV-AMT                  PIC S9(11)V99 COMP-3.
         05  CLNP-APRV-UNUSED-BAL           PIC S9(11)V9(2) COMP-3.
         05  CLNP-CAPTL-PURPOSE             PIC 9(2).
         05  CLNP-LN-CODE                   PIC 9(2).
         05  CLNP-INT-CODE                  PIC 9(2).
         05  CLNP-IN-DE-INT-CODE            PIC S99V999.
         05  CLNP-INT-ADJ-INTVL             PIC 9(2).
         05  CLNP-APRV-DATE                 PIC 9(07).                  
         05  CLNP-APRV-INTVL-DATA           PIC 9(08).                  
         05  CLNP-EACH-INTVL-DATA           PIC 9(08).                  
         05  CLNP-REPLY-INTVL               PIC 9(1).
         05  CLNP-RTRN-TYPE                 PIC 9(03).                  
         05  CLNP-AFTER-TERM-RTRN-TYPE      PIC 9(1).
         05  CLNP-TERM-RECV-INTVL           PIC 9(3).
         05  CLNP-TERM-BGN-RTRN-DATE        PIC 9(07).                  
         05  CLNP-RECV-INT-TYPE             PIC 9(2).
         05  CLNP-GURN-NO                   PIC S9(3) COMP-3.
         05  CLNP-STOP-CODE                 PIC 9(1).
         05  CLNP-STATUS                    PIC 9(1).
040317   05  CLNP-FILLER                    PIC X(313).                 
      *-----------------------------------------------------------------
       01  APRV-TABLE.                                                  
         05  APRV-A                       PIC  X(3)  VALUE '10A'.       
         05  APRV-B                       PIC  X(3)  VALUE '11B'.       
         05  APRV-C                       PIC  X(3)  VALUE '12C'.       
         05  APRV-D                       PIC  X(3)  VALUE '13D'.       
         05  APRV-E                       PIC  X(3)  VALUE '14E'.       
         05  APRV-F                       PIC  X(3)  VALUE '15F'.       
         05  APRV-G                       PIC  X(3)  VALUE '16G'.       
         05  APRV-H                       PIC  X(3)  VALUE '17H'.       
         05  APRV-I                       PIC  X(3)  VALUE '18I'.       
         05  APRV-J                       PIC  X(3)  VALUE '19J'.       
         05  APRV-K                       PIC  X(3)  VALUE '20K'.       
         05  APRV-L                       PIC  X(3)  VALUE '21L'.       
         05  APRV-M                       PIC  X(3)  VALUE '22M'.       
         05  APRV-N                       PIC  X(3)  VALUE '23N'.       
         05  APRV-O                       PIC  X(3)  VALUE '24O'.       
         05  APRV-P                       PIC  X(3)  VALUE '25P'.       
         05  APRV-Q                       PIC  X(3)  VALUE '26Q'.       
         05  APRV-R                       PIC  X(3)  VALUE '27R'.       
         05  APRV-S                       PIC  X(3)  VALUE '28S'.       
         05  APRV-T                       PIC  X(3)  VALUE '29T'.       
         05  APRV-U                       PIC  X(3)  VALUE '30U'.       
         05  APRV-V                       PIC  X(3)  VALUE '31V'.       
         05  APRV-W                       PIC  X(3)  VALUE '32W'.       
         05  APRV-X                       PIC  X(3)  VALUE '33X'.       
         05  APRV-Y                       PIC  X(3)  VALUE '34Y'.       
         05  APRV-Z                       PIC  X(3)  VALUE '35Z'.       
       01  APRV-TABLER    REDEFINES  APRV-TABLE.                        
         02  APRV-TAB  OCCURS     26    TIMES  INDEXED BY N.            
           05  APRV-KEY                    PIC  9(2).                   
           05  APRV-DATA                   PIC  X(1).                   
980720*--------/ ＣＡＬＬ－ＣＦＬ９２０１參數傳輸區　/--------------  
"      01  LINK-CF-AREA.                                                
080425   05  LINK-CF-LENGTH            PIC S9(4)  COMP VALUE +51.       
"        05  LINK-CF-TXN-ID            PIC X(04) VALUE SPACES.          
"        05  LINK-P-DATA.                                               
"          10  LINK-P-ACC-NO           PIC 9(12) VALUE ZEROS.           
"          10  LINK-P-OPEN-DAY         PIC 9(07) VALUE ZEROS.           
"          10  LINK-P-ID-NO            PIC X(10) VALUE ZEROS.           
980720     10  LINK-P-SER              PIC 9(02) VALUE ZEROS.           
981119     10  LINK-P-DB-CR-CODE       PIC 9(01) VALUE ZEROS.           
080425     10  LINK-P-ID               PIC X(04) VALUE ZEROS.           
080425     10  LINK-P-AMT              PIC 9(11) VALUE ZEROS.           
000218*#####################################################            
"     *#   連線大額通貨現金交易明細檔〔ＣＦＭＪＩＤ〕   ##            
"     *#   （ＫＳＤＳ，長度＝１００；ＫＥＹ值＝２６）   ##            
"     *#####################################################            
"            COPY  CFKMJIB.                                             
"      01  WK-TXN-SEQ.                                                  
"        05  WK-TXN-SEQ-1            PIC 9(05) VALUE ZEROS.             
"        05  WK-TXN-SEQ-2            PIC 9(05) VALUE ZEROS.             
981223*---傳真交易檔                                                  
"          COPY        PBKFAX.                                          
991203*---有效利率參數傳輸區                                          
"      01  INTCAL-P.                                                    
"          05  INTCAL-FUN-CODE                 PIC 9(02) VALUE ZEROS.   
"          05  INTCAL-ACC-NO                   PIC 9(12) VALUE ZEROS.   
"          05  INTCAL-LN-ID-NO.                                         
"            10  INTCAL-APRV-NO                PIC 9(11) VALUE ZEROS.   
"            10  INTCAL-LN-SER-NO              PIC 9(04) VALUE ZEROS.   
"          05  INTCAL-ACC-CODE                 PIC 9(02) VALUE ZEROS.   
"          05  INTCAL-INT-CODE                 PIC 9(02) VALUE ZEROS.   
"          05  INTCAL-RATE-ADJ                 PIC 9(02) VALUE ZEROS.   
"          05  INTCAL-RTRN-CODE                PIC 9(01) VALUE ZEROS.   
"          05  INTCAL-RTRN-INTVL               PIC 9(02) VALUE ZEROS.   
"          05  INTCAL-AFTER-TERM-RTRN-TYPE     PIC 9(01) VALUE ZEROS.   
"          05  INTCAL-INT-RATE            PIC 9(02)V999999 VALUE ZEROS. 
"          05  INTCAL-EIR                 PIC 9(02)V999999 VALUE ZEROS. 
"          05  INTCAL-IN-DE-RATE           PIC S9(02)V999 VALUE ZEROS.  
"          05  INTCAL-LN-BAL                   PIC 9(11) VALUE ZEROS.   
"          05  INTCAL-CARRYING-AMT             PIC 9(11) VALUE ZEROS.   
"          05  INTCAL-EACH-RTRN-AMT            PIC 9(11) VALUE ZEROS.   
"          05  INTCAL-L-RECV-INT-DATE          PIC 9(07) VALUE ZEROS.   
"          05  INTCAL-TERM-BGN-RTRN-DATE       PIC 9(07) VALUE ZEROS.   
"          05  INTCAL-DUE-DATE                 PIC 9(07) VALUE ZEROS.   
"          05  INTCAL-NOT-YET-RTRN-FREE        PIC S9(11) VALUE ZEROS.  
"          05  INTCAL-FIX-FREE                 PIC 9(11) VALUE ZEROS.   
"          05  INTCAL-FIX-COST                 PIC 9(11) VALUE ZEROS.   
991203     05  INTCAL-RETURN-CODE              PIC X(04) VALUE SPACES.  
010130*---------------金融機構代號檢算工作區                          
"      01  FINCAL-ID-CHECK-WORK-AREA.                                   
"          05  WK-TEMP-SUM                  PIC 9(03) VALUE ZEROS.      
"          05  FILLER        REDEFINES WK-TEMP-SUM.                     
"              10  WK-TEMP-SUM-D1           PIC 9(01).                  
"              10  WK-TEMP-SUM-D2           PIC 9(01).                  
"              10  WK-TEMP-SUM-D3           PIC 9(01).                  
"          05  WS-RECV-ID                   PIC X(06) VALUE SPACES.     
"          05  FILLER        REDEFINES WS-RECV-ID.                      
"              10  WK-BANK-ID.                                          
"                  20  WS-RECV-BK-1         PIC 9(01).                  
"                  20  WS-RECV-BK-2         PIC 9(01).                  
"                  20  WS-RECV-BK-3         PIC 9(01).                  
"              10  WK-FINCAL-BRANCH.                                    
"                  20  WS-RECV-BK-4         PIC 9(01).                  
"                  20  WS-RECV-BK-5         PIC 9(01).                  
"                  20  WS-RECV-BK-6         PIC 9(01).                  
010130     05  WK-BRANCH-CHK                PIC 9(01).                  
      *--------------- COPYBOOK:FILE,COMMINICATION-AREA                 
       01  FILLER  PIC X(16) VALUE '//   LNM-DB   //'.  COPY LNKLNM.    
       01  FILLER  PIC X(16) VALUE '//   LNP-DB   //'.  COPY LNKLNP.    
       01  FILLER  PIC X(16) VALUE '//   LNL-DB   //'.  COPY LNKLNL.    
       01  FILLER  PIC X(16) VALUE '//   LNR-DB   //'.  COPY LNKLNR.    
       01  FILLER  PIC X(16) VALUE '//   LNI-DB   //'.  COPY LNKLNI.    
901109 01  FILLER  PIC X(16) VALUE '//  LNLBINT-F //'. COPY LNKLBINT.   
930730 01  FILLER  PIC X(16) VALUE '//  LNCLOSE   //'. COPY LNKCLOSE.   
       01  FILLER  PIC X(16) VALUE '//   TIF-P    //'.  COPY PBKTIFP.   
       01  FILLER  PIC X(16) VALUE '//   TEL-R    //'.  COPY PBKTEL.    
       01  FILLER  PIC X(16) VALUE '//   BIF-P    //'.  COPY LNKBIFP.   
980506 01  FILLER  PIC X(16) VALUE '// INT-GROUP  //'.  COPY LNKINT7.   
       01  FILLER  PIC X(16) VALUE '//   LOG-P    //'.  COPY LNKLOG.    
991203 01  FILLER  PIC X(16) VALUE '//  LNFCOST   //'. COPY LNKCOST.    
991203 01  FILLER  PIC X(16) VALUE '//  LNKACIF7  //'. COPY LNKACIF7.   
001019 01  FILLER  PIC X(16) VALUE '//  LNKAGDI   //'. COPY LNKAGDI.    
010130 01  FILLER  PIC X(16) VALUE '///LNJRLTM-F ///'.  COPY LNKJRLTM.  
B00309*------------/ＣＦＭＪＩＢ５檔/--------------------------------*
B00309 01  FILLER  PIC X(16) VALUE '// CFMJIB5-F  //'. COPY CFKMJIB5.   
       01  FILLER  PIC X(16) VALUE '//  MSG-OUT   //'.  COPY LNKMSGO.   
           10  WS-MSG-DATA-JOURNAL    REDEFINES  MSG-P-SP-OTHER-DATA.
             15  WJ-MSG-TXN-TIME.
                20  WJ-MSG-HH                 PIC 9(2).
                20  WJ-MSG-F1                 PIC X.
                20  WJ-MSG-MM                 PIC 9(2).
                20  WJ-MSG-F2                 PIC X.
                20  WJ-MSG-SS                 PIC 9(2).
             15  WJ-MSG-ACC-NO                PIC X(12).
             15  WJ-MSG-LN-ID-NO              PIC X(15).                
             15  WJ-MSG-AMT                   PIC **,***,***,**9.99.
             15  WJ-MSG-INT-RATE              PIC 9(2).999.
791114       15  WJ-MSG-DATE                  PIC 9(7).
             15  WJ-MSG-ACC-CODE              PIC 9(2).
             15  WJ-MSG-COR-TYPE              PIC 9.
             15  WJ-MSG-SUP-CODE              PIC 9(2).
040317       15  WJ-MSG-SUP-CODEX REDEFINES WJ-MSG-SUP-CODE   PIC X(02).
860925       15  WJ-MSG-150-MARK              PIC X.                    
"            15  FILLER                      PIC X(417).                
911101*---------------中文錯誤訊息                                    
"          05  MSG-T282      REDEFINES MSG-P-OUT-MSG.                   
"              10  MSG-T282-LINE1           PIC X(52).                  
"              10  MSG-T282-LINE2           PIC X(52).                  
040317         10  FILLER    REDEFINES MSG-T282-LINE2.                  
"                  20  FILLER               PIC X(31).                  
"                  20  T282-L2-AMT0         PIC ZZZ,ZZZ,ZZ9.            
"                  20  FILLER               PIC X(10).                  
911101         10  MSG-T282-LINE3           PIC X(52).                  
"              10  MSG-T282-LINE4           PIC X(52).                  
"              10  FILLER                   PIC X(292).                 
      *---------------連動存款交易〔轉支傳票〕輸出格式----------------
010426*---------------〔轉支傳票〕填寫欄                              
"        05  MSG-P-T6A4           REDEFINES MSG-P-OUT-MSG.              
"            10  T6A4-ACCT-CODE             PIC X(05).                  
"            10  T6A4-ACCT-NAME             PIC X(20).                  
"            10  FILLER           REDEFINES T6A4-ACCT-NAME.             
"                20  T6A4-ACCT-0E           PIC X(01).                  
"                20  FILLER                 PIC X(18).                  
"                20  T6A4-ACCT-0F           PIC X(01).                  
"            10  T6A4-TXN-YY                PIC 9(03).                  
"            10  FILLER           REDEFINES T6A4-TXN-YY   PIC X(03).    
"            10  T6A4-TXN-MM                PIC 9(02).                  
"            10  FILLER           REDEFINES T6A4-TXN-MM   PIC X(02).    
"            10  T6A4-TXN-DD                PIC 9(02).                  
"            10  FILLER           REDEFINES T6A4-TXN-DD   PIC X(02).    
"            10  T6A4-SUB-CODE              PIC X(04).                  
"            10  T6A4-SUB-NAME              PIC X(20).                  
"            10  FILLER           REDEFINES T6A4-SUB-NAME.              
"                20  T6A4-SUB-0E            PIC X(01).                  
"                20  FILLER                 PIC X(18).                  
"                20  T6A4-SUB-0F            PIC X(01).                  
"            10  T6A4-ACC-NO                PIC X(12).                  
"            10  FILLER           REDEFINES T6A4-ACC-NO.                
"                20  T6A4-ACC-BR-CODE       PIC X(03).                  
"                20  T6A4-ACC-DEP-BUS       PIC X(03).                  
"                20  FILLER       REDEFINES T6A4-ACC-DEP-BUS.           
"                    30  T6A4-DEP-CODE      PIC X(01).                  
"                    30  T6A4-BUSINS-CODE   PIC X(02).                  
"                20  T6A4-ACC-SER           PIC X(06).                  
"            10  T6A4-CUST-NAME             PIC X(22).                  
"            10  FILLER           REDEFINES T6A4-CUST-NAME.             
"                20  T6A4-CUST-0E           PIC X(01).                  
"                20  FILLER                 PIC X(20).                  
"                20  T6A4-CUST-0F           PIC X(01).                  
010426       10  T6A4-TXN-AMT               PIC $$,$$$,$$$,$$9.99.      
"            10  T6A4-MEMO-1                PIC X(30).                  
"            10  FILLER           REDEFINES T6A4-MEMO-1.                
"                20  T6A4-MEMO1-0E          PIC X(01).                  
"                20  FILLER                 PIC X(28).                  
"                20  T6A4-MEMO1-0F          PIC X(01).                  
"            10  T6A4-MEMO-2                PIC X(30).                  
"            10  FILLER           REDEFINES T6A4-MEMO-2.                
"                20  T6A4-LN-ACC-NO         PIC X(12).                  
"                20  FILLER                 PIC X(01).                  
"                20  T6A4-APRV-NO           PIC X(11).                  
"                20  T6A4-DASH1             PIC X(01).                  
"                20  T6A4-LN-SER-NO         PIC X(04).                  
"                20  FILLER                 PIC X(01).                  
"            10  T6A4-MEMO-3                PIC X(30).                  
"            10  FILLER                     PIC X(303).                 
010426*---------------〔轉支傳票〕認證欄                              
"        05  MSG-P-T6A5           REDEFINES MSG-P-OUT-MSG.              
"            10  T6A5-LINE-01.                                          
"                20  T6A5-ACC-TYPE          PIC X(01).                  
"                20  T6A5-TM-NO             PIC X(02).                  
"                20  T6A5-TXN-SER-NO        PIC 9(05).                  
"                20  T6A5-TXN-CODE          PIC X(04).                  
"                20  FILLER                 PIC X(01).                  
"                20  T6A5-ACC-NO1           PIC X(12).                  
"                20  FILLER                 PIC X(01).                  
"                20  T6A5-TR-MEMO           PIC X(14).                  
080116           20  FILLER                 PIC X(02).                  
"                20  FILLER                 PIC X(01).                  
"                20  T6A5-TR-AMT            PIC **,***,***,**9.99.      
"                20  FILLER                 PIC X(01).                  
"                20  T6A5-OUT-DATE          PIC 9(07).                  
"                20  T6A5-TR-CODE           PIC X(02).                  
"                20  T6A5-TR-PB-BAL         PIC X(17).                  
"                20  FILLER                 PIC X(03).                  
080116       10  T6A5-LINE-02.                                          
"                20  FILLER                 PIC X(38).                  
"                20  T6A5-SUP-CODE          PIC X(02).                  
"                20  T6A5-SUP-NAME          PIC X(10).                  
"                20  FILLER                 PIC X(11).                  
"                20  T6A5-TXN-TIME.                                     
"                    30  T6A5-TIME-HH       PIC X(02).                  
"                    30  T6A5-TIME-X1       PIC X(01).                  
"                    30  T6A5-TIME-MM       PIC X(02).                  
"                    30  T6A5-TIME-X2       PIC X(01).                  
"                    30  T6A5-TIME-SS       PIC X(02).                  
"                20  FILLER                 PIC X(01).                  
"                20  T6A5-TELLER-CODE       PIC X(02).                  
"                20  T6A5-TELLER-NAME       PIC X(10).                  
"                20  FILLER                 PIC X(08).                  
"            10  FILLER                     PIC X(320).                 
971222*---------------傳票認證（Ｔ６３５）                            
"          05  T635-GROUP              REDEFINES MSG-P-OUT-MSG.         
"              10  T635-CUST-NAME                PIC X(40).             
080116         10  T635-SUP-DATA.                                       
"                  20  T635-SUP-TITLE            PIC X(07).             
"                  20  T635-SUP-CODE             PIC X(02).             
"                  20  T635-SUP-NAME             PIC X(10).             
080116         10  T635-TELLER-DATA.                                    
"                  20  T635-TELLER-TITLE         PIC X(07).             
"                  20  T635-TELLER-CODE          PIC X(02).             
"                  20  T635-TELLER-NAME          PIC X(10).             
971222         10  T635-AMT-TYPE                 PIC X(01).             
"              10  T635-SEQ-NO.                                         
"                  20  T635-TM-CODE              PIC 9(02).             
"                  20  T635-TXN-SEQ-NO           PIC 9(05).             
"              10  T635-TXN-ID-CODE              PIC 9(04).             
"              10  MSG-P-SP-OTHER-ITEM.                                 
                   15  MSG-P-SP-ACC-NO           PIC 9(12).             
                   15  MSG-P-SP-OLD-LN-ID-NO     PIC 9(15).             
                   15  MSG-P-SP-ACC-CODE         PIC 9(2).              
                   15  MSG-P-SP-CURRENCY-CODE    PIC 9(2).              
                   15  MSG-P-SP-AMT              PIC **,***,***,**9.99. 
791114             15  MSG-P-SP-DUE-DATE         PIC 9(7).              
                   15  MSG-P-SP-SUP-CODE         PIC X(2).              
960720             15  MSG-P-SP-BAD-TYPE         PIC X(2).              
"                  15  MSG-P-SP-BAD-MARK         PIC X(1).              
050302             15  T635-ADD-RSN              PIC X(02).             
"                  15  T635-PAYABLE-AMT          PIC **,***,***,**9.    
"                  15  T635-PROCES-TYPE          PIC X(02).             
"                  15  T635-RELIEF-TYPE          PIC X(02).             
                   15  MSG-P-SP-NEW-LN-ID-NO     PIC 9(15).             
050302             15  MSG-P-SP-TXN-DATE         PIC 9(07).             
"                  15  MSG-P-SP-TXN-TIME-HH      PIC 9(02).             
"                  15  MSG-P-SP-FILLER1          PIC X(01).             
"                  15  MSG-P-SP-TXN-TIME-MM      PIC 9(02).             
"                  15  MSG-P-SP-FILLER2          PIC X(01).             
"                  15  MSG-P-SP-TXN-TIME-SS      PIC 9(02).             
"              10  FILLER                        PIC X(300).            
001019*---------------提醒字句--T6K0( L=90)                           
"          05  MSG-NOTICE-T6K0    REDEFINES MSG-P-OUT-MSG.              
"              10  T6K0-ATTEN               PIC X(08).                  
"              10  T6K0-TXN-ID              PIC X(04).                  
"              10  T6K0-DASH1               PIC X(01).                  
"              10  T6K0-TYPE                PIC X(01).                  
"              10  T6K0-STATEMENT           PIC X(76).                  
"              10  FILLER                   PIC X(410).                 
001019*---------------轉收轉支傳票--T116( L=145)                      
"          05  MSG-SLIP-T116      REDEFINES MSG-P-OUT-MSG.              
"              10  T116-ACCT-DATA           PIC X(21).                  
"              10  FILLER         REDEFINES T116-ACCT-DATA.             
"                  20  T116-ACCT-CODE       PIC X(10).                  
"                  20  T116-ACCT-NAME       PIC X(10).                  
"                  20  FILLER               PIC X(01).                  
"              10  T116-TXN-DATE            PIC 9(07).                  
"              10  T116-SUB-DATA            PIC X(20).                  
"              10  FILLER         REDEFINES T116-SUB-DATA.              
"                  20  T116-ACC-NO          PIC X(12).                  
"                  20  T116-DASH1           PIC X(01).                  
"                  20  T116-APRV-NO         PIC X(07).                  
001019         10  T116-TXN-AMT             PIC $$,$$$,$$$,$$9.99.      
"              10  T116-MEMO1               PIC X(30).                  
"              10  T116-MEMO2               PIC X(30).                  
"              10  FILLER         REDEFINES T116-MEMO2.                 
"                  20  T116-M2-TIME-A       PIC X(08).                  
"                  20  T116-M2-TIME         PIC X(06).                  
"                  20  FILLER               PIC X(02).                  
"                  20  T116-M2-TELLER-A     PIC X(08).                  
"                  20  T116-M2-TELLER       PIC X(02).                  
"                  20  FILLER               PIC X(04).                  
"              10  T116-MEMO3               PIC X(20).                  
"              10  FILLER                   PIC X(355).                 
041211*------------/大額通額交易送資料至端末系統/--------------------*
"        05  MSG-P-OUT-T105-920  REDEFINES  MSG-P-OUT-MSG.              
"            10  MSG-P-T105-ACNO            PIC X(12).                  
"            10  MSG-P-T105-PER-ID          PIC X(10).                  
"            10  MSG-P-T105-SER             PIC 9(02).                  
"            10  FILLER                     PIC X(476).                 
050614*---------------新網銀回覆電文----------------------------------
"        05  T6A9-GROUP           REDEFINES MSG-P-OUT-MSG.              
"            10  T6A9-RETURN-CODE           PIC X(04).                  
"            10  T6A9-LN-BAL                PIC 9(11).                  
"            10  T6A9-N-RECV-DATE           PIC 9(07).                  
"            10  T6A9-N-RECV-INT            PIC 9(11).                  
"            10  T6A9-N-RTRN-AMT            PIC 9(11).                  
"            10  T6A9-PB-MARK               PIC X(01).                  
"            10  T6A9-PB-BAL                PIC 9(11).                  
"            10  T6A9-WARNING               PIC X(102).                 
"            10  FILLER                     PIC X(342).                 
961206*--------------< T638 >-------------------------------------------
       01  FILLER                          PIC X(8)  VALUE 'MSG-DT2:'.
       01  W-MSG-DATA2.
         05  W-MSG2-ACC-NO                 PIC X(12).
         05  W-MSG2-LN-ID-NO               PIC 9(15).
040317   05  W-MSG2-LN-ID-NO-X  REDEFINES  W-MSG2-LN-ID-NO    PIC X(15).
         05  W-MSG2-RECV-TYPE              PIC 9(1).
         05  W-MSG2-FINE                   PIC **,***,***,**9.99.
040317   05  W-MSG2-FINE-X      REDEFINES  W-MSG2-FINE        PIC X(17).
980518   05  W-MSG2-TXN-DATE               PIC 9(07).                   
"        05  W-MSG2-TXN-TIME-HH            PIC 9(02).                   
"        05  W-MSG2-FILLER1                PIC X(01).                   
"        05  W-MSG2-TXN-TIME-MM            PIC 9(02).                   
"        05  W-MSG2-FILLER2                PIC X(01).                   
"        05  W-MSG2-TXN-TIME-SS            PIC 9(02).                   
080116   05  T638-SUP-NAME                 PIC X(10) VALUE SPACES.      
"        05  T638-TELLER-NAME              PIC X(10) VALUE SPACES.      
980518   05  W-MSG2-INT-BGN-DATE           PIC 9(07).                   
"        05  W-MSG2-INT-END-DATE           PIC 9(07).                   
"        05  W-MSG2-RECV-AMT               PIC **,***,***,**9.99.       
"        05  W-MSG2-TOTAL-INT-AMT          PIC **,***,***,**9.99.       
"        05  W-MSG2-SUP-CODE               PIC 9(02).                   
040317   05  W-MSG2-SUP-CODE-X  REDEFINES  W-MSG2-SUP-CODE    PIC X(02).
980508   05  W-MSG2-RECV-RTRN-DIFF         PIC X(17).                   
"        05  W-MSG2-TELLER-CODE            PIC X(02).                   
"        05  W-MSG2-150-MARK               PIC X(01).                   
"        05  W-MSG2-REASON                 PIC X(02).                   
981223   05  W-MSG2-FAX-NO                 PIC X(04).                   
050720   05  W-MSG2-SCH-RTRN               PIC X(12).                   
         05  W-MSG2-AML-DATA.                                           
B00309       10  W-MSG2-AML-BLACK              PIC X(16).               
B00309       10  W-MSG2-AML-BLACK-COMM         PIC X(01).               
B00309       10  W-MSG2-AML-CODE-C             PIC X(17).               
B00309       10  W-MSG2-AML-CODE               PIC X(14).               
       01  WS-MSG-CTZN-DATA-SLIP.
           05  W-MSG-CTZN-ACC-NO           PIC 9(12).
           05  W-MSG-CTZN-APRV-NO          PIC 9(11).
           05  W-MSG-CTZN-LN-SER-NO        PIC 9(4).
           05  W-MSG-CTZN-AMT              PIC **,***,***,**9.99.
860925     05  W-MSG-CTZN-150-MARK         PIC X.
"          05  FILLER                      PIC X(443).
       01  WS-MSG-CTZN-DATA-JOURNAL   REDEFINES  WS-MSG-CTZN-DATA-SLIP.
           05  WC-MSG-TXN-TIME.
              10  WC-MSG-HH                 PIC 9(2).
              10  WC-MSG-F1                 PIC X.
              10  WC-MSG-MM                 PIC 9(2).
              10  WC-MSG-F2                 PIC X.
              10  WC-MSG-SS                 PIC 9(2).
           05  WC-MSG-CTZN-ACC-NO           PIC 9(12).
           05  WC-MSG-CTZN-APRV-NO          PIC 9(11).
           05  WC-MSG-CTZN-LN-SER-NO        PIC 9(4).
           05  WC-MSG-CTZN-AMT              PIC **,***,***,**9.99.
860925     05  WC-MSG-CTZN-150-MARK         PIC X.
"          05  FILLER                       PIC X(435).
860925 01  WK-M-LAUNDERING.                                             
"        05  WK-M-L-AMT-TYPE               PIC X.                       
"        05  WK-M-L-SEQ-NO                 PIC X(7).                    
"        05  WK-M-L-TXN-ID-CODE            PIC 9(4).                    
"        05  WK-M-L-ACC-ID-NO              PIC 9(12).                   
"        05  WK-M-L-LN-ID-NO               PIC 9(15).                   
"        05  WK-M-L-FILLER1                PIC X      VALUE  SPACES.    
"        05  WK-M-L-TXN-AMT                PIC **,***,***,**9.99.       
"        05  WK-M-L-FILLER2                PIC X     VALUE  SPACES.     
"        05  WK-M-L-CASH-AMT               PIC **,***,***,**9.99.       
"        05  WK-M-L-SUP-KEY                PIC X(2).                    
"        05  WK-M-L-FILLER3                PIC X      VALUE  SPACES.    
961206   05  WK-M-L-DATE                   PIC 9(07) VALUE ZEROS.       
      *--------------- COPYBOOK:FIXED-VALUE                             
920321 01  FILLER  PIC X(16) VALUE '//  INT-WORK  //'. COPY LNKINTW.    
       01  FILLER  PIC X(16) VALUE '//   COM-M    //'. COPY LNKCOMM.    
       01  FILLER  PIC X(16) VALUE '//   TWA-M    //'. COPY LNKTWAM.    
       01  FILLER  PIC X(16) VALUE '//   CTF-M    //'. COPY LNKCTFM.    
       01  FILLER  PIC X(16) VALUE '//   BIFM-M   //'. COPY LNKBIFMM.   
       01  FILLER  PIC X(16) VALUE '//   LNM-M    //'. COPY LNKLNMM.    
       01  FILLER  PIC X(16) VALUE '//   LNP-M    //'. COPY LNKLNPM.    
       01  FILLER  PIC X(16) VALUE '//   LNL-M    //'. COPY LNKLNLM.    
       01  FILLER  PIC X(16) VALUE '//   LNR-M    //'. COPY LNKLNRM.    
       01  FILLER  PIC X(16) VALUE '//   LNI-M    //'. COPY LNKLNIM.    
       01  FILLER  PIC X(16) VALUE '//   INT-M    //'. COPY LNKINTM.    
       01  FILLER  PIC X(16) VALUE '//   SEA-M    //'. COPY LNKSEAM.    
       01  FILLER  PIC X(16) VALUE '//   DAT-M    //'. COPY LNKDATM.    
       01  FILLER  PIC X(16) VALUE '//   MSG-M    //'. COPY LNKMSGM.    
980720 01  FILLER  PIC X(16) VALUE '//  LNKTBL5   //'. COPY LNKTBL5.    
990831 01  FILLER  PIC X(16) VALUE '//  LNKTBL7   //'. COPY LNKTBL7.    
050614 01  FILLER  PIC X(16) VALUE '//  CTKDTBL   //'. COPY CTKDTBL.    
141124*-----------《限制清償設定檔LNFLMTR(6317)》-------------------
"      01  FILLER  PIC X(16) VALUE '//  LNFLMTR   //'. COPY LNKLMTR.    
"      01  FILLER  PIC X(16) VALUE '//  LMTR-WORK //'.                  
"      01  LNFLMTR-WORK-AREA.                                           
AAAA       05  LMTR-LENGTH                  PIC S9(04) COMP VALUE +200. 
"          05  WLMTR-READ-MARK              PIC X(01) VALUE SPACES.     
"          05  WLMTR-END-DATE               PIC 9(07) VALUE ZEROS.      
"          05  FILLER             REDEFINES WLMTR-END-DATE.             
"              10  WLMTR-END-YY             PIC 9(03).                  
"              10  WLMTR-END-MM             PIC 9(02).                  
"              10  WLMTR-END-DD             PIC 9(02).                  
"          05  WK-WORK1-NO                  PIC 9(05) VALUE ZEROS.      
"          05  WK-WORK2-NO                  PIC 9(05) VALUE ZEROS.      
"          05  WK-WORK3-NO                  PIC 9(05) VALUE ZEROS.      
"      01  WLMTR-GROUP                      PIC X(21) VALUE ZEROS.      
"      01  FILLER                 REDEFINES WLMTR-GROUP.                
"          05  WLMTR-LMT-DATE     OCCURS 03 TIMES     PIC 9(07).        
------*-----------------------------------------------------------------
       LINKAGE SECTION.
140725*---------------被６６３連動工作區                              
140725 01  DFHCOMMAREA.                                                 
"          05  COMM-663-ACC-NO              PIC 9(12).                  
"          05  COMM-663-LN-ID-NO            PIC 9(15).                  
"          05  COMM-663-RECV-AMT            PIC S9(11)V9(2).            
"          05  COMM-663-RECV-HI-TYPE        PIC 9(01).                  
"          05  COMM-663-INT-BGN-DATE        PIC 9(07).                  
"          05  COMM-663-INT-END-DATE        PIC 9(07).                  
"          05  COMM-663-INT                 PIC S9(11)V9(2).            
"          05  COMM-663-FINE                PIC S9(11)V9(2).            
"          05  COMM-663-OVERDUE-RECV        PIC 9(02).                  
"          05  COMM-663-PB-ACC-NO           PIC 9(12).                  
"          05  COMM-663-SUP-CODE            PIC 9(02).                  
"          05  COMM-663-AMT                 PIC S9(11)V9(2).            
"          05  COMM-663-ADV-NO              PIC 9(07).                  
"          05  COMM-663-RELIEF-TYPE         PIC X(02).                  
"          05  COMM-663-REV-SER-NO          PIC 9(10).                  
140725     05  COMM-663-RETURN-CODE         PIC X(04).                  
890309     COPY  LNKTWA.                                                
"          COPY  LNKCTF.                                                
050614     COPY  ISKCWA.                                                
890309     COPY  LNKREV.                                                
------*-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           PERFORM     100-TXN-INITIAL-RTN         THRU    100-EXIT.
           PERFORM     200-INPUT-DATA-CHECK-RTN    THRU    200-EXIT.
           PERFORM     300-FILE-RETRIEVAL-RTN      THRU    300-EXIT.
           PERFORM     400-CHECK-PROC-RTN          THRU    400-EXIT.
           PERFORM     500-FILE-PROC-RTN           THRU    500-EXIT.
           PERFORM     800-TXN-END-RTN             THRU    800-EXIT.
       000-EXIT.                                                        
           EXIT.                                                        
------*---------------起始定址、設定LOG BEFORE欄位------------------
       100-TXN-INITIAL-RTN.
           PERFORM     9000-PROG-INIT-RTN.                              
140725     PERFORM     110-6783-TRANS-CTF        THRU  110-EXIT.        
           MOVE        'LNL671'            TO      TWA-MAP-PROG-NAME.
           MOVE        SPACE               TO      TWA-SAP-PROG-NAME.
810821     IF          TWA-C-TXN-REVERSE
           THEN
             MOVE      REV-TELLER-CODE     TO      WS-TELLER-CODE.
860925     MOVE        SPACE              TO   W-MSG2-150-MARK.         
"          MOVE        SPACE              TO   W-MSG-CTZN-150-MARK.     
"          MOVE        SPACE              TO   WC-MSG-CTZN-150-MARK.    
"          MOVE        SPACE              TO   WJ-MSG-150-MARK.         
900221     MOVE        SPACES             TO   WK-LNCNTR-ID-NO(1).      
"          MOVE        ZEROS              TO   WK-LNCNTR-ACC-NO(1).     
"          MOVE        ZEROS              TO   WK-LNCNTR-NO(1).         
"          MOVE        ZEROS              TO   WK-LNCNTR-NT-BAL(1).     
040317     MOVE        WK-CONTR-DATA(1)   TO   WK-CONTR-DATA(2).        
"          MOVE        WK-CONTR-DATA(1)   TO   WK-CONTR-DATA(3).        
"          MOVE        WK-CONTR-DATA(1)   TO   WK-CONTR-DATA(4).        
931216     MOVE        TWA-TXN-ID-CODE    TO   WK-TXN-ID-CODE.          
931216     MOVE        TWA-ACC-TXN-SEQ-NO TO   WK-ACC-TXN-SEQ-NO.       
940517     MOVE        CTF-TXN-SER-NO     TO   WK-CTF-TXN-SER-NO.       
040327     PERFORM     9171-LN-INT-AREA-CLEAR.                          
050614*-----------------------------------------------------------------
"     *•擷取ＥＩＢ系統日期（＝２位預留欄＋２位民國年＋３位太陽日）   
"     *　　　－－＞轉換為正常７位民國年月日                           
"     *•擷取ＣＷＡ：確認〔當日帳／次日帳〕、〔日版／夜版〕           
"     *-----------------------------------------------------------------
"          PERFORM     991-GET-EIBDATE-RTN       THRU  991-EXIT.        
"          EXEC        CICS    ADDRESS CWA(ADDRESS OF CWA)    END-EXEC. 
050614*---------------設定ＬＯＧ：存款帳號／系統日期                  
"          MOVE        CTF-TR-ACC-NO-1           TO   AFT-AF-AMND.      
"          MOVE        WK-EIB-DATE               TO   AFT-RECV-DUE-DATE.
TEST  *---------------起始檢核顯示                                    
"          IF        ( WK-EIB-DATE < 1141125 )                          
"          THEN                                                         
"                      MOVE    'Y'               TO   DISP-MARK         
"                      MOVE    TWA-TXN-ID-CODE   TO   DISP-TXN-ID-CODE  
"                      MOVE    TWA-MAP-PROG-NAME TO   DISP-PROG-NAME    
"                      MOVE    TWA-TM-ID-NO      TO   DISP-TM-ID-NO     
"                      MOVE    TWA-TXN-TIME      TO   DISP-TXN-TIME     
"                      MOVE '======= TXN-BEGIN:' TO   DISP-ERR-MARK1    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP-ERROR-END)  END-EXEC  
"          END-IF.                                                      
B00309     MOVE  CTF-AML-CODE     TO   WS-AML-CODE.                     
B00309     MOVE  CTF-PAY-NAME     TO   WS-TXN-CUST-NAME.                
B00309     MOVE  CTF-BLACK-LIST   TO   WS-BLACK-LIST.                   
       100-EXIT.
           EXIT.
------*---------------輸入資料檢驗------------------------------------
       200-INPUT-DATA-CHECK-RTN.
990604     MOVE        CTF-ACC-NO           TO   SW-TEMP-ACC-NO.        
990604     IF  ( TWA-TXN-ID-CODE  = 6710 OR 6740 OR 6770 )              
990604     AND ( CTF-RECV-TYPE    = 1  )                                
990604     AND ( SW-921-ACC-NO         )                                
"          AND  (TWA-C-SUP-OFF)                                         
"          THEN                                                         
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '本戶為９２１地震災民重建家園專案貸款戶'            
"                                             TO   MSG-T282-LINE1       
"            MOVE '請查明並收回代墊息'      TO   MSG-T282-LINE2       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
990118*---------------現金＆非沖正交易：檢核〔櫃員現金〕資料          
"          IF        ( TWA-TXN-ID-CODE = 6710 OR 6722 OR 6740 OR 6752 ) 
"                AND ( NOT  TWA-C-TXN-REVERSE )                         
"          THEN                                                         
"                      NEXT   SENTENCE                                  
"          ELSE                                                         
"                      GO  TO  200-C.                                   
      *    ENDIF                                                        
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN(601-FILE-NOT-OPEN)                       
"                      NOTFND(200-C)                                    
"                      ERROR(200-C)                                     
"                      END-EXEC.                                        
"          MOVE    TWA-BR-CODE         TO      TIF-W-BR-CODE.           
990118     MOVE    TWA-TELLER-CODE     TO      TIF-W-TELLER-CODE.       
"          EXEC  CICS  READ  DATASET('PBTIF')                           
"                            INTO   (TIF)                               
"                            RIDFLD (TIF-W-KEY)                         
"                            LENGTH (TIF-W-LENG)                        
"                            EQUAL                                      
"                      END-EXEC.                                        
990118*---------------檢核：櫃員現金超過８０萬＆現金交易次數超過５筆  
"          IF        ( TIF-NO-80  NOT NUMERIC )                         
"                 OR ( TIF-NO-80  <  5 )                                
"          THEN                                                         
"                      GO  TO  200-C.                                   
           MOVE        'D241'              TO      MSG-P-OUT-CODE.      
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
      *---------------其他檢核----------------------------------------
990118 200-C.                                                           
           PERFORM     210-CASE-BEG        THRU    210-CASE-END.
       200-EXIT.
           EXIT.
------*---------------輸入項目檢核------------------------------------
       210-CASE-BEG.
           COMPUTE     CTF-W-LOG-AMT = CTF-RECV-AMT + CTF-INT-AMT       
                                                    + CTF-FINE.         
           MOVE        CTF-W-LOG-AMT     TO     LOG-AMT.
B00309     IF        ( TWA-TXN-ID-CODE   =      6722 )                  
               AND   ( CTF-W-LOG-AMT     >      500000 )                
               AND   (TWA-C-TXN-NOT-REVERSE)                            
           THEN                                                         
               MOVE    SPACES               TO   MSG-T282               
               MOVE    '符合洗錢防制請執行６７１現金收本息　　　　'   
                                            TO   MSG-T282-LINE1         
               PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
B00309     IF        ( TWA-TXN-ID-CODE   =      6752 )                  
               AND   ( CTF-W-LOG-AMT     >      500000 )                
               AND   (TWA-C-TXN-NOT-REVERSE)                            
           THEN                                                         
               MOVE    SPACES               TO   MSG-T282               
               MOVE    '符合洗錢防制請執行６７４混合收本息　　　　'   
                                            TO   MSG-T282-LINE1         
               PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  210-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  210-CASE-REVERSE.
           GO  TO      210-CASE-ERR.
      *---------------輸入資料檢驗：正常交易--------------------------
       210-CASE-NORMAL.
      *----------------金額檢核                                       
810421     IF          (CTF-RECV-AMT      < 0)
           THEN
           MOVE        'M679'              TO      MSG-P-OUT-CODE
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-INT-AMT < 0)
           THEN
           MOVE        'M640'              TO      MSG-P-OUT-CODE
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-FINE < 0)
           THEN
           MOVE        'M60I'              TO      MSG-P-OUT-CODE
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           COMPUTE     W-AMT-ROUNDED ROUNDED = CTF-INT-AMT.
           COMPUTE     W-DIFF-ROUNDED = W-AMT-ROUNDED - CTF-INT-AMT.
           IF          (W-DIFF-ROUNDED  NOT =  0)
           THEN
             MOVE      'M640'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           COMPUTE     W-AMT-ROUNDED ROUNDED = CTF-FINE.
           COMPUTE     W-DIFF-ROUNDED = W-AMT-ROUNDED - CTF-FINE.
           IF          (W-DIFF-ROUNDED  NOT =  0)
           THEN
             MOVE      'M60I'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
981223*---------------傳真序號不是數值　　　　　　　　　　　　　      
"          IF          CTF-BUSINS-CODE   NOT   NUMERIC                  
"          THEN                                                         
"              MOVE    SPACES               TO   MSG-T282               
050614         MOVE    'M6XX傳真序號不是數值，請重新執行此交易'       
"                                           TO   MSG-T282-LINE1         
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"          IF        ( CTF-BUSINS-CODE  NOT =  ZEROS )                  
"                AND ( TWA-TXN-ID-CODE  NOT =  6770 )                   
           THEN                                                         
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE    '此為傳真交易，交易代號有誤，請輸入正確代號'   
"                                           TO   MSG-T282-LINE1         
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"     *---------------傳真交易不得連動存摺扣款                        
"          IF        ( CTF-BUSINS-CODE  NOT =  ZEROS )                  
"                AND ( CTF-TR-ACC-NO-1  NOT =  ZEROS )                  
"          THEN                                                         
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE    '傳真交易不得連動存摺扣款，請重新執行此交易'   
"                                           TO   MSG-T282-LINE1         
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"     *---------------傳真交易檔檢核　　　                            
"          IF          CTF-BUSINS-CODE   NOT   =   ZEROS                
"                AND   TWA-C-TXN-NOT-REVERSE                            
"          THEN                                                         
"                      PERFORM   2010-FAX-CODE-CHK-RTN THRU  2010-EXIT. 
050614*-----------------------------------------------------------------
"     *網銀不開放1.結帳後（３點半後次日帳）2.夜版ＣＩＣＳ3.假日 
"     *-----------------------------------------------------------------
"          IF        ( TWA-BR-CODE  =  '026' )                          
"              AND  (( CWA-C-ATM-TOMORROW )                             
"                 OR ( CWA-C-RRDS-NIGHT   )                             
"                 OR ( TWA-TXN-DAT7  >  WK-EIB-DATE ))                  
"          THEN                                                         
"                      MOVE    'M6B1'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
931216*---------------連動存摺扣款檢查                                
           IF          CTF-TR-ACC-NO-1   NOT   =   ZEROS                
           THEN                                                         
             IF   NOT (CTF-RECV-TYPE =  1 OR 2 OR 4 OR 7 )              
             THEN                                                       
               MOVE    'M686'              TO      MSG-P-OUT-CODE       
931216         PERFORM 9146-LN-ERR-MSG-OUT-RTN.                         
931216*---------------連動存摺〔０〕：為取得存款戶名／管轄行          
090701     MOVE        SPACES               TO   SV-CUST-NAME.          
           IF        ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR )          
                 AND ( CTF-TR-ACC-NO-1   NOT =  ZEROS )                 
050614           AND ( TWA-BR-CODE       NOT =  '026' )                 
           THEN                                                         
               MOVE    CTF-TR-ACC-NO-1     TO    LPB-ACC-NO             
"              MOVE    CTF-LN-ID-NO        TO    LPB-LN-ID-NO           
               MOVE    0                   TO    LPB-FUN-CODE           
               MOVE    CTF-W-LOG-AMT       TO    LPB-DECR-AMT           
               MOVE   '0000'               TO    LPB-RC-CODE            
050122                 PERFORM 9401-PB-LINK-RTN       THRU  9401-EXIT   
940824                 MOVE    LPB-CUST-NAME     TO   SV-CUST-NAME.     
920903*----------------收回原因                                       
920903     MOVE        CTF-CAPTL-PURPOSE    TO   LNL-RTRN-RSN.          
971222     MOVE        SPACES               TO   CHK-MARK1.             
"          SET         IDX09                TO   1.                     
"          SEARCH      RTRN-RSN-TABLE                                   
"             WHEN   ( LNL-RTRN-RSN  =  TBL-RTRN-RSN(IDX09) )           
"                      MOVE    'Y'          TO   CHK-MARK1.             
      *****ENDSEARCH                                                    
"          IF        ( CHK-MARK1  NOT =  'Y' )                          
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M6XX鍵入之「收回原因」代碼超出設定範圍，'          
"                                                TO   MSG-T282-LINE1    
"            MOVE '請查明確認後，重新輸入！'   TO   MSG-T282-LINE2    
"            MOVE '（代碼請參閱６７７交易操作手冊）'                  
"                                                TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
971222*---------------「收回類別／收回原因」交叉檢核                  
"          IF        ( CTF-RECV-TYPE          =  '3' )                  
"                AND ( CTF-CAPTL-PURPOSE  NOT =  '71' )                 
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE '「收回類別」鍵入『３展期』，' TO MSG-T282-LINE1    
"            MOVE '而「收回原因」非鍵入『７１展期』，'                
"                                                TO   MSG-T282-LINE2    
"            MOVE '，請查明確認後重新登打！'   TO   MSG-T282-LINE2    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"          IF        ( CTF-RECV-TYPE          =  '5' )                  
"                AND ( CTF-CAPTL-PURPOSE  NOT =  '81' )                 
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE '「收回類別」鍵入『５轉催收款項』，'                
"                                                TO   MSG-T282-LINE1    
"            MOVE '而「收回原因」非鍵入『８１轉列催收』，'            
"                                                TO   MSG-T282-LINE2    
"            MOVE '，請查明確認後重新登打！'   TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"          IF        ( CTF-RECV-TYPE          =  '6' )                  
"                AND ( CTF-CAPTL-PURPOSE  NOT =  '80' )                 
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE '「收回類別」鍵入『６轉籍移出』，'                  
"                                                TO   MSG-T282-LINE1    
"            MOVE '而「收回原因」非鍵入『８０其他』，'                
"                                                TO   MSG-T282-LINE2    
"            MOVE '，請查明確認後重新登打！'   TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"          IF          CTF-CAPTL-PURPOSE =  '90'                        
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"                      MOVE '鍵入之「收回原因」代碼９０，免責債權'    
"                                           TO   MSG-T282-LINE1         
"                      MOVE '暫時未啟用，待法務處制訂作業規章後，'    
"                                           TO   MSG-T282-LINE2         
"                      MOVE '才可開放鍵檔。'   TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"     *---------------檢核：免責債權                                  
"          IF          CTF-CAPTL-PURPOSE =  '90'                        
"                AND ( CTF-RELIEF-TYPE  =  '00' OR SPACES )             
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"                      MOVE '鍵入之「收回原因」代碼９０，免責債權'    
"                                           TO   MSG-T282-LINE1         
"                      MOVE '種類需鍵入，請查明確認後，重新輸入！'    
"                                           TO   MSG-T282-LINE2         
"                      MOVE '（代碼請參閱６７７交易操作手冊）'        
"                                           TO   MSG-T282-LINE3         
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"          IF        ( CTF-RELIEF-TYPE  =  '00' OR SPACES )             
"          THEN                                                         
"                      GO  TO  210-NEXT-RTN.                            
"          MOVE        SPACE                TO   CHK-MARK1.             
"          SET         IDX30                TO   1.                     
"          SEARCH      RELIEF-TYPE-TABLE                                
"             WHEN   ( CTF-RELIEF-TYPE  =  TBL-RELIEF-TYPE(IDX30) )     
"                      MOVE    'Y'          TO   CHK-MARK1.             
"     *    ENDSEARCH                                                    
"          IF        ( CHK-MARK1  NOT =  'Y' )                          
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
050614       MOVE 'M6XX鍵入之「免責債權」代碼超出設定範圍，'          
"                                                TO   MSG-T282-LINE1    
"            MOVE '請查明確認後，重新輸入！'   TO   MSG-T282-LINE2    
"            MOVE '（代碼請參閱６７７交易操作手冊）'                  
"                                                TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"      210-NEXT-RTN.                                                    
      *---------------檢核〔起迄日〕                                  
901109     IF        ( CTF-RECV-TYPE         =  7        )              
"              AND  (( CTF-INT-BGN-DATE-N-R  NOT NUMERIC )              
"                 OR ( CTF-INT-BGN-DATE-N-R  =  ZEROS    )              
901109            OR ( CTF-INT-END-DATE-N-R  NOT NUMERIC )              
901109            OR ( CTF-INT-END-DATE-N-R  =  ZEROS    ))             
           THEN                                                         
                       MOVE  'M60B'    TO    MSG-P-OUT-CODE             
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
050627*---------------控管：迄日不可＜起日                            
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
"                AND ( CTF-INT-BGN-DATE-N  NUMERIC )                    
"                AND ( CTF-INT-BGN-DATE-N  >  ZEROS )                   
"                AND ( CTF-INT-END-DATE-N  NUMERIC )                    
"                AND ( CTF-INT-END-DATE-N  >  ZEROS )                   
"                AND ( CTF-INT-END-DATE-N  <  CTF-INT-BGN-DATE-N  )     
"          THEN                                                         
"                      MOVE  'M60L'    TO    MSG-P-OUT-CODE             
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
"     *    ENDIF                                                        
010309*---------------控管：繳息迄日不可＞３０年後，避免分行鍵錯誤收  
"          MOVE        TWA-TXN-DAT7         TO   WK-WORK-DATE.          
"          ADD         30                   TO   WK-WORK-YY.            
"          IF        ( CTF-INT-END-DATE-N-R   >  WK-WORK-DATE )         
"          THEN                                                         
"                      MOVE  SPACES              TO   MSG-T282          
050614       MOVE 'M6XX〔收息迄日〕＞〔交易日＋３０年〕，不合常理'    
"                                                TO   MSG-T282-LINE1    
"                      MOVE  '請查明確認！'    TO   MSG-T282-LINE2    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
010830     IF       (  TWA-C-TXN-NOT-REVERSE )                          
"               AND (  LNL-ACC-CODE   =  48 OR 58 )                     
"          THEN                                                         
"            IF      ( CTF-INT-AMT  >  ZEROS )                          
"                 OR ( CTF-FINE     >  ZEROS )                          
"            THEN                                                       
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE 'M210A 呆帳科目，利息金額及違約金額欄位不得輸入，'
"                                                TO   MSG-T282-LINE1    
"              MOVE '      請查明後重新輸入'   TO   MSG-T282-LINE2    
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
060412*---------------〔６轉籍移出〕檢核代收票據                      
"          IF        ( CTF-RECV-TYPE  =  '6' )                          
"          THEN                                                         
"                      PERFORM 260-BCVOT-CHK-RTN      THRU  260-EXIT.   
"     *    ENDIF                                                        
      *---------------檢核結束                                        
           GO  TO      210-CASE-END.
------*---------------輸入資料檢驗：沖正交易--------------------------
       210-CASE-REVERSE.
           GO  TO      210-CASE-END.
       210-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      210-CASE-END.
       210-CASE-END.
           EXIT.
------*---------------傳真檔檢核--------------------------------------
981223 2010-FAX-CODE-CHK-RTN.                                           
"          EXEC CICS HANDLE CONDITION                                   
"                    NOTOPEN(2010-NOTOPEN)                              
"                    NOTFND(2010-EXIT-99)                               
"                    ERROR(2010-ERR)                                    
"          END-EXEC.                                                    
"          MOVE   ZEROS             TO      FAX-KEY.                    
"          MOVE   TWA-TXN-DAT7      TO      FAX-DATE.                   
"          MOVE   TWA-BR-CODE       TO      FAX-BR.                     
"          MOVE   CTF-BUSINS-CODE   TO      FAX-NUMBER.                 
"          MOVE   ZEROS             TO      FAX-TIME.                   
"          MOVE   ZEROS             TO      FAX-TM-CODE.                
"          MOVE   ZEROS             TO      FAX-TXN-SEQ-NO.             
"          MOVE   0                 TO      WK-FAX-TXN-STATUS.          
"      2010-FAX-CODE-CHK-RTN-N.                                         
"          EXEC     CICS  READ      DATASET('PBFAX')                    
"                                   INTO(FAX-REC)                       
"                                   LENGTH(FAX-LENGTH)                  
"                                   RIDFLD(FAX-KEY)                     
"                                   GTEQ                                
"          END-EXEC.                                                    
"          IF     TWA-TXN-DAT7          =     FAX-DATE                  
"            AND  TWA-BR-CODE           =     FAX-BR                    
"            AND  CTF-BUSINS-CODE       =     FAX-NUMBER                
"          THEN                                                         
"                 NEXT  SENTENCE                                        
"          ELSE                                                         
"             GO   TO   2010-EXIT-99.                                   
"          IF     FAX-TXN-DB-CR  =  1                                   
"          THEN                                                         
"           IF    FAX-TXN-STATUS =  1                                   
"           THEN                                                        
"            MOVE   1                     TO  WK-FAX-TXN-STATUS         
"            MOVE   FAX-TR-OUT-ACC        TO  WK-FAX-ACC                
"           ELSE                                                        
"            MOVE   9                     TO  WK-FAX-TXN-STATUS.        
"                                                                       
"          ADD       1                    TO   FAX-TXN-SER.             
"          GO   TO   2010-FAX-CODE-CHK-RTN-N.                           
"      2010-EXIT-99.                                                    
"          IF       WK-FAX-TXN-STATUS  =  1                             
"          THEN                                                         
"            NEXT  SENTENCE                                             
"          ELSE                                                         
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE    '此傳真序號尚未轉帳支出，請認後重新登錄　　'   
"                                           TO   MSG-T282-LINE1         
"              MOVE    '傳真序號'         TO   MSG-T282-LINE2         
"              MOVE     CTF-BUSINS-CODE     TO   MSG-T282-LINE3         
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"      2010-EXIT.                                                       
"           EXIT.                                                       
"      2010-NOTOPEN.                                                    
"          MOVE    SPACES               TO   MSG-T282.                  
"          MOVE    '此為傳真交易，檔案未開啟，請通知資訊處　　'       
"                                           TO   MSG-T282-LINE1.        
"          PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.          
"      2010-ERR.                                                        
"          MOVE    SPACES               TO   MSG-T282.                  
"          MOVE    '此為傳真交易，其它錯誤，請通知資訊處　　　'       
"                                           TO   MSG-T282-LINE1.        
"          PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.          
"      2010-NOTFND.                                                     
"          MOVE    SPACES               TO   MSG-T282.                  
"          MOVE    '此為傳真交易，找不到資料，查明後請重新執行'       
"                                       TO   MSG-T282-LINE1.            
"          PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.          
------*---------------〔６轉籍移出〕檢核代收票據----------------------
060412 260-BCVOT-CHK-RTN.                                               
"          MOVE   '（BCVOT代收票據資料檔）'  TO   MSG-FILE-LINE2.   
"          EXEC        CICS    HANDLE  CONDITION                        
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      NOTFND (260-EXIT)                
"                                      ERROR  (910-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
060412*---------------讀檔                                            
"          MOVE        CTF-ACC-NO      TO   BCVOT-ACC-ID-CODE.          
"          MOVE        00000000        TO   BCVOT-ISSUE-DATE.           
"          MOVE        000             TO   BCVOT-BR-CODE.              
"          MOVE        00000000        TO   BCVOT-RG-DATE.              
"          MOVE        00000           TO   BCVOT-SER-NO.               
"          MOVE        +36             TO   COMP-LENGTH.                
"          EXEC        CICS    READ    DATASET  ('BCVOT')               
"                                      INTO     (BCVOT)                 
"                                      RIDFLD   (BCVOT-ID-CODE)         
"                                      KEYLENGTH(COMP-LENGTH)           
"                                      GTEQ                             
"                      END-EXEC.                                        
060421*---------------帳戶尚有代收票據                                
"          IF        ( CTF-ACC-NO  =  BCVOT-ACC-ID-CODE )               
"                AND ( TWA-C-SUP-OFF )                                  
"          THEN                                                         
"                      MOVE    SPACES            TO   MSG-T282          
"            MOVE '帳戶尚有代收票據，不能轉籍移出（收回類別６），'    
"                                                TO   MSG-T282-LINE1    
"            MOVE '須先以「４５１／４５１代收票據撤票」交易解除後，'  
"                                                TO   MSG-T282-LINE2    
"            MOVE '才能轉籍移出，請查明確認。' TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"     *    ENDIF                                                        
"      260-EXIT.                                                        
"          EXIT.                                                        
------*---------------擷取主檔、檢驗狀況------------------------------
       300-FILE-RETRIEVAL-RTN.
           MOVE        1                   TO       SW-FINE-CHARGE.
           MOVE        0                   TO       END-DATE-SW.
030224     IF   NOT    TWA-TXN-ID-CODE     =        6783
"          THEN
                       PERFORM     9002-LN-OPEN-RTN
                       PERFORM     9105-LN-DBS-ERR-HDL-RTN.
      *    ENDIF.
           PERFORM     9016-LN-DBS-M2-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
980819     IF        ( LNM-TAX-ID  =  ZEROS OR SPACES OR LOW-VALUE )    
"          THEN                                                         
"            MOVE      LNM-CUST-ID          TO   WK-ID-NO               
"          ELSE                                                         
"            MOVE      LNM-TAX-ID           TO   WK-TAX-ID.             
      *    ENDIF                                                        
050614*---網銀輸入ＩＤ與ＬＮＭＩＤ檢核（右靠左補０），以加強安全性    
"          IF        ( TWA-BR-CODE      =  '026' )                      
"                AND ( CTF-CUST-ID  NOT =  LOW-VALUE )                  
"                AND ( CTF-CUST-ID  NOT =  SPACES )                     
"                AND ( CTF-CUST-ID  NOT =  ZEROS  )                     
"          THEN                                                         
"            IF      ( WK-ID-NO  NOT =  CTF-CUST-ID )                   
"            THEN                                                       
"                      MOVE    'M17I'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
041211*-----------------------------------------------------------------
"     *設定〔大額通貨〕使用資料：開戶日、ＩＤ序號、ＩＤ（左靠右補空白 
"     *-----------------------------------------------------------------
"          MOVE        LNM-CREAT-LNM-DATE   TO   WK-LNM-CREAT-DATE.     
"          MOVE        LNM-CFM-SER          TO   WK-LNM-CFM-SER.        
"          IF        ( LNM-TAX-ID  =  LOW-VALUE OR SPACES OR ZEROS )    
"          THEN                                                         
"                      MOVE    LNM-CUST-ID       TO   WLNM-CUST-ID      
"          ELSE                                                         
"                      MOVE    SPACES            TO   WLNM-CUST-ID      
"                      MOVE    LNM-TAX-ID        TO   WLNM-TAX-ID       
"          END-IF.                                                      
131011*-----------------------------------------------------------------
"     *設定：〔６７７－６轉籍〕警示帳戶禁止轉籍／ＩＤ左靠右補空白     
"     *-----------------------------------------------------------------
"          IF       (( TWA-TXN-ID-CODE    =   6770 )                    
"               AND  ( CTF-RECV-TYPE      =   '6'  ))                   
"          THEN                                                         
"            IF      ( LNM-TAX-ID  =  LOW-VALUE OR SPACES OR ZEROS )    
"            THEN                                                       
131202                 MOVE    SPACES        TO    CFBLACK              
131202                 MOVE    WLNM-CUST-ID  TO    CFBLACK-ID           
131202                 PERFORM 380-CFBLACK-CHK-RTN THRU  380-EXIT       
"            ELSE                                                       
131202                 MOVE    SPACES        TO    CFBLACK              
131202                 MOVE    WLNM-TAX-ID   TO    CFBLACK-ID           
131202                 PERFORM 380-CFBLACK-CHK-RTN THRU  380-EXIT       
           END-IF.                                                      
      *---------------                                                  
           PERFORM     300-LN-DB-P2-RTN       THRU 300-P2-EXIT.
           PERFORM     9116-LNP-STATUS-CHK-RTN-N.
970729*---------------約定採日息計算特別處理                          
"          MOVE        SPACES               TO   TBL27-MARK.            
"          SET         IDX27                TO   1.                     
"          SEARCH      TBL27-TABLE                                      
"            WHEN   (( TBL27-ACC-NO(IDX27)   =  LNM-ACC-NO  )           
"                AND ( TBL27-APRV-NO(IDX27)  =  '999999999' ))          
"              OR                                                       
"                   (( TBL27-ACC-NO(IDX27)   =  LNM-ACC-NO  )           
"                AND ( TBL27-APRV-NO(IDX27)  =  LNP-APRV-NO ))          
"                      MOVE    'Y'          TO   TBL27-MARK.            
"     *    ENDSEARCH                                                    
      *---------------擷取LNL及檢驗狀況                             
           PERFORM     9047-LN-DBS-L2-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
960625     IF        ( LNL-ACC-CODE  =  48 OR 49 )                      
"               AND  ( LNM-STATUS    =  0  )                            
"          THEN                                                         
"                      MOVE   1     TO    LNM-STATUS.                   
000225     IF       (( LNM-ACC-BR-CODE    =    101 )                    
                AND  ( LNP-LN-CODE        =    04  ))                   
                OR  (( LNM-ACC-BR-CODE    =    101 )                    
                AND  ( LNP-LN-CODE        =    64  )                    
                AND  ( LNL-ACC-CODE       =    23  ) )                  
"          THEN                                                         
"                      MOVE   'Y'   TO    F-101-MARK.                   
"          PERFORM     9112-LNM-STATUS-CHK-RTN-A.                       
           PERFORM     9134-LNL-STATUS-CHK-RTN-ERR-E.
060824     IF       (  TWA-C-TXN-NOT-REVERSE   )                        
"               AND (  LNL-ACC-CODE     =  58  )                        
"               AND (  TWA-BR-CODE NOT  =  101 )                        
"          THEN                                                         
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE 'M210A 國宅呆帳科目請劃報單由國宅科處理'          
"                                                TO   MSG-T282-LINE1    
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
090527     IF       (  TWA-C-TXN-NOT-REVERSE   )                        
"               AND (  LNL-ACC-CODE     =  58  )                        
"          THEN                                                         
               IF   (  CTF-INT-BGN-DATE-N-R NOT =  ZEROS )              
               AND  (  CTF-INT-END-DATE-N-R NOT =  ZEROS )              
               THEN                                                     
"                      MOVE    SPACES               TO   MSG-T282       
"                      MOVE 'M210A 國宅呆帳科目利息起訖日請空敲'      
"                                                TO   MSG-T282-LINE1    
"                   PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT. 
020524*---------------催收轉銷呆帳：檢核〔原貸科目〕                  
"          IF        ( LNL-ACC-CODE       =  '47' OR '57' )             
"                AND ( CTF-CAPTL-PURPOSE  =  '50'         )             
"                AND ( LNP-LN-CODE    NOT =  '78'         )             
"                AND ( TWA-C-TXN-NOT-REVERSE              )             
"          THEN                                                         
"            IF      ( LNL-ORGL-ACC  NOT NUMERIC  )                     
"                 OR ( LNL-ORGL-ACC  =  ZEROS     )                     
"                 OR ( LNL-ORGL-ACC  =  '47' OR '48' OR '49' )          
"            THEN                                                       
"                      MOVE  SPACES              TO   MSG-T282          
"            MOVE '缺漏有效之『原貸科目』，不可轉銷呆帳，'            
"                                                TO   MSG-T282-LINE1    
"            MOVE '請先以６２６交易變更項目「７２原科目」，'          
"                                                TO   MSG-T282-LINE2    
"            MOVE '補入正確之『原貸科目』後，再轉銷呆帳！'            
"                                                TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU  9146A-EXIT. 
010803*---------------〔呆帳〕不可收取〔利息／違約金〕                
"          IF       (( LNL-ACC-CODE  =  '48' OR '49'  )                 
141203             OR ( LNM-ACC-BUSINS-CODE      =  '69'                
141203            AND   LNL-ACC-CODE         = '58' ) )                 
"              AND  (( CTF-INT-AMT  >  ZEROS )                          
"                 OR ( CTF-FINE     >  ZEROS ))                         
"          THEN                                                         
"                      MOVE    SPACES            TO   MSG-T282          
"            MOVE '〔呆帳〕不得收取『利息』或『逾期息／違約金』'      
"                                                TO   MSG-T282-LINE1    
"            MOVE '請查明後重新輸入！'         TO   MSG-T282-LINE2    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU  9146A-EXIT. 
060824*---------------〔國宅呆帳〕不可收大於餘額，以676 TYPE:6收息  
060824     IF        ( LNM-ACC-BUSINS-CODE      =  '69'
                 AND   LNL-ACC-CODE         = '58' )
"                AND ( TWA-C-TXN-NOT-REVERSE       )                    
           THEN
"              IF    ( CTF-RECV-AMT >  LNL-LN-BAL )                     
"              THEN                                                     
"                      MOVE    SPACES            TO   MSG-T282          
"              MOVE '〔收回國宅呆帳金額大於現欠金額』'                
"                                                TO   MSG-T282-LINE1    
"              MOVE '請查明後重新輸入！'         TO   MSG-T282-LINE2  
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU  9146A-EXIT. 
      *--------------------------------------------*----------
061206     IF       (  TWA-C-TXN-NOT-REVERSE   )                        
"               AND (  LNL-ACC-CODE     =  '47' OR '48' )               
"               AND (  LNM-SCH-TMP      =  'S' )                        
"               AND (  LNP-LN-CODE      =  '55' )                       
"          THEN                                                         
"              MOVE    SPACES               TO   MSG-T282               
"              MOVE 'M210X 就學貸款簡易轉呆帳注意需２８分帳'          
"                                                TO   MSG-T282-LINE1    
"              MOVE 'M210X 使用６３０－２７解除' TO MSG-T282-LINE2    
"              PERFORM 9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
061206     IF       (  TWA-C-TXN-NOT-REVERSE   )                        
"               AND (  LNL-ACC-CODE     =  '47' OR '48' )               
"               AND (  LNP-LN-CODE      =  '55' )                       
"               AND (  LNP-CT-ISSUE-DATE   =  '1061031' )               
"          THEN                                                         
                       MOVE   'S'       TO    WK-SIMP-CODE.
      *---------------協議：改用新到期日                              
940701     IF        ( LNL-BC-COLM     =  'C' OR 'D' )                  
"                AND ( LNL-DUE-DATE-N  NUMERIC )                        
"                AND ( LNL-DUE-DATE-N  >  ZEROS )                       
           THEN
             MOVE      LNL-DUE-DATE-N      TO      WKS-DUE-DATE
           ELSE
             MOVE      LNL-DUE-DATE        TO      WKS-DUE-DATE.
991221     IF          LNL-CARRYING-AMT  NOT NUMERIC                    
           THEN                                                         
                       MOVE   ZEROS      TO   LNL-CARRYING-AMT.         
991221     IF          LNL-NOT-YET-RTRN-FREE  NOT NUMERIC               
           THEN                                                         
                       MOVE   +0         TO   LNL-NOT-YET-RTRN-FREE.    
991221     IF          LNL-RECVABL-ADJ-AMT    NOT NUMERIC               
           THEN                                                         
                       MOVE   +0         TO   LNL-RECVABL-ADJ-AMT.      
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
             MOVE      LNL-L-RECV-INT-DATE    TO   BEF-L-RECV-INT-DATE
A01114       MOVE      LNL-INSUR-AMT          TO   BEF-INSUR-AMT
900821       MOVE      LNL-L-RECV-INT-DATE-R  TO   WK-L-RECV-INT-DATE   
             MOVE      LNL-RECV-INT-DATE      TO   BEF-RECV-INT-DATE
             MOVE      LNL-NEXT-INT-ADJ-DATE  TO   BEF-INT-ADJ-DATE
             MOVE      LNL-INT-RATE           TO   BEF-INT-RATE
             MOVE      LNL-LN-BAL             TO   BEF-LN-BAL
891019       MOVE      LNL-INT-INCR-DECR-RATE TO   BEF-INCR-ALNRATE     
920527       MOVE      LNP-INT-ADJ-INTVL      TO   BEF-ADJ-ALNRATE      
970922       MOVE      LNL-LAST-RTRN-DATE-R   TO   BEF-LAST-RTRN-DATE   
             MOVE      LNL-EACH-RTRN-AMT      TO   BEF-EACH-RTRN-AMT
             MOVE      LNL-NEXT-RTRN-DATE     TO   BEF-NEXT-RTRN-DATE
             MOVE      LNL-STATUS             TO   BEF-LNL-STATUS
             MOVE      LNP-RTRN-INTVL         TO   BEF-RTRN-INTVL
             MOVE      LNM-LAST-TXN-DATE      TO   BEF-LAST-TXN-DATE
950526       MOVE      LNP-INT-CODE           TO   BEF-LNP-INT-CODE     
950526       MOVE      LNL-INT-CODE           TO   BEF-LNL-INT-CODE     
950526       MOVE      LNP-IN-DE-INT-CODE     TO   BEF-LNP-IN-DE-RATE   
991203       MOVE      LNL-CARRYING-AMT       TO   BEF-CARRYING-AMT     
991203       MOVE      LNL-NOT-YET-RTRN-FREE  TO  BEF-NOT-YET-RTRN-FREE 
991203       MOVE      LNL-RECVABL-ADJ-AMT    TO   BEF-RECVABL-ADJ-AMT  
991203       MOVE      LNL-EIR                TO   BEF-EIR              
             MOVE      LNM-BAL-SW             TO   BEF-BAL-SW           
101025       MOVE      LNL-RTRN-RSN           TO   BEF-LNL-RTRN-RSN.    
950912*---------------就學貸款短期墊款檢查----------------------------
120710*---------------就學貸款檢查是否為疫後補貼戶--------------------
           IF          LNP-C-ENT-HGHER-SCH-LN                           
           THEN                                                         
120710         PERFORM 330-SCHSUDY-PROC-RTN   THRU 330-EXIT             
               PERFORM 320-ACLNID-CHECK-RTN   THRU 320-EXIT.            
961226*---------------就學貸款信保基金理賠----------------------------
           IF          LNP-C-ENT-HGHER-SCH-LN                           
           AND         LNL-ACC-CODE    =  49                            
           THEN                                                         
               PERFORM 3210-TRUST-AMT-RTN   THRU 3210-EXIT.             
931216*-----------------------------------------------------------------
      *檢驗〔連動扣款存款帳號〕：臨櫃繳款限為「約定扣款帳號」         
      *　　　　　　　　　　　　　網銀繳款控管不可為０                 
      *-----------------------------------------------------------------
050614     IF        ( TWA-BR-CODE        NOT =  '026' )                
                AND  ( TWA-C-ID-RCVHI-TR  OR  TWA-C-ID-SLIP-TR )        
                AND  ( CTF-TR-ACC-NO-1    NOT =  ZEROS )                
                AND  ( LNL-AUTO-ACC-NO-1  NOT =  CTF-TR-ACC-NO-1 )      
           THEN                                                         
                       MOVE    'M61I'            TO   MSG-P-OUT-CODE    
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
050614     IF        ( TWA-BR-CODE  =  '026' )                          
"              AND                                                      
"                   (( CTF-TR-ACC-NO-1  NOT NUMERIC )                   
"                 OR ( CTF-TR-ACC-NO-1  =  ZEROS    ))                  
"          THEN                                                         
"                      MOVE    'M61I'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
970609*---------------檢驗：就學貸款催收呆帳只由營業部收              
"          IF        ( LNL-ACC-CODE  =  47 OR 48 OR 49 )                
"               AND  ( LNP-LN-CODE   =  55 )                            
B00101          AND  ( TWA-BR-CODE  NOT =  101 )                        
"          THEN                                                         
"                      MOVE  SPACES              TO   MSG-T282          
B00101                 MOVE  '就學貸款催收及呆帳請洽學貸科　        ' 
"                                                TO   MSG-T282-LINE1    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
931216*---------------檢驗：信用卡貸款只由數金處機器收（帳掛營業部）  
121017     IF        ( LNM-ACC-NO(1:6)  =  '101366' )                   
           THEN                                                         
             IF      ( TWA-BR-CODE  =  101 )                            
030331           AND ( TWA-TM-CODE  =  33 OR 34 OR 35 OR 36 )           
             THEN                                                       
                       NEXT    SENTENCE                                 
             ELSE                                                       
               MOVE      'M605'              TO      MSG-P-OUT-CODE     
931216       PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
           MOVE        ZEROS               TO      WSI.
           MOVE        ZERO                TO      WSI-FOR-INT-AMT.
           MOVE        ZERO                TO      WSI-RECVABL-AMT.
           MOVE        ZERO                TO      WSI-RECVED-AMT.
           MOVE        ZERO                TO      WSI-INT-FINE.
930730*---------------控管：勞工住宅貸款處份押品收回                  
"          IF        ( CTF-RECV-TYPE      =  '1' )                      
971222           AND ( CTF-CAPTL-PURPOSE  =  '16' )                     
"          THEN                                                         
"                      PERFORM 310-LNCLOSE-CHECK-RTN  THRU  310-EXIT.   
981125*---------------特別協議：部分本息協議分期攤還不收逾期息        
"          IF        ( LNP-LN-CODE   =  24 )                            
                OR   ( LNL-ACC-CODE  =  53 OR 54 )                      
"          THEN                                                         
"                      PERFORM 358-LN24-CHK THRU 358-EXIT.              
A10423*---------------特別協議：國宅固定攤還金額（網銀夜間使用不檢查  
"          IF        ( TWA-BR-CODE  NOT =  026  )                       
           THEN
"                      PERFORM 3581-LN28-CHK THRU 3581-EXIT.            
141124*---------------【３客製限制清償】讀ＬＭＴＲ檔設定相關參數      
"          IF        ( LNP-LMT-RTN-MARK = '3' )                         
"          THEN                                                         
"                      PERFORM 305-LMTR-READ-RTN      THRU  305-EXIT    
"          END-IF.                                                      
980731*---------------限制提前清償專案                                
981119     COMPUTE   W-LIMIT-DATE-1Y  = LNL-1ST-LN-DATE  +  10000.      
981119     COMPUTE   W-LIMIT-DATE-2Y  = LNL-1ST-LN-DATE  +  20000.      
981119     COMPUTE   W-LIMIT-DATE-3Y  = LNL-1ST-LN-DATE  +  30000.      
"          IF  ( LNP-LN-CODE  =  21 OR 47 )                             
990712     AND ( LNP-CT-ISSUE-DATE = 0980707 OR 0981104 OR 0990305      
010615                            OR 0990701 OR 1000112 OR 1010611 )    
981119     AND ( W-LIMIT-DATE-2Y  NOT <  TWA-TXN-DAT7 )                 
"          THEN                                                         
"            MOVE      'Y'             TO      W-LIMIT-RTRN-CASE.       
"          IF  ( W-LIMIT-RTRN-CASE = 'Y'  )                             
"          AND ( CTF-RECV-TYPE   = '1' OR '4' OR '7' )                  
"          AND  (TWA-C-SUP-OFF)                                         
"          THEN                                                         
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '限制清償期間：第一年收取清償金額１％，'            
"                                             TO   MSG-T282-LINE1       
"            MOVE '　　　　　　　第二年收取清償金額０。５％之違約金'  
"                                             TO   MSG-T282-LINE2       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
981111*限制提前清償專案------------（９８１１０２）（存單開發日）   
"          IF  ( LNP-CT-ISSUE-DATE = 0981102 )                          
981119     AND ( W-LIMIT-DATE-2Y  NOT <  TWA-TXN-DAT7 )                 
"          AND  ( LNP-ACC-CODE    =  38 )                               
"          AND ( LNP-LN-CODE   NOT   =   03      )                      
"          AND ( LNP-LN-CODE   NOT   =   55      )                      
"          AND ( LNP-LN-CODE   NOT   =   13      )                      
"          AND ( LNP-LN-CODE   NOT   =   67      )                      
"          THEN                                                         
"            MOVE      '2'             TO      W-LIMIT-RTRN-CASE.       
981119     IF  ( LNP-CT-ISSUE-DATE = 0981101 )                          
981119     AND ( W-LIMIT-DATE-1Y  NOT <  TWA-TXN-DAT7 )                 
"          AND  ( LNP-ACC-CODE    =  38 )                               
"          AND ( LNP-LN-CODE   NOT   =   03      )                      
"          AND ( LNP-LN-CODE   NOT   =   55      )                      
"          AND ( LNP-LN-CODE   NOT   =   13      )                      
"          AND ( LNP-LN-CODE   NOT   =   67      )                      
"          THEN                                                         
"            MOVE      '1'             TO      W-LIMIT-RTRN-CASE.       
981119     IF  ( LNP-CT-ISSUE-DATE = 0981103 )                          
981119     AND ( W-LIMIT-DATE-3Y  NOT <  TWA-TXN-DAT7 )                 
"          AND  ( LNP-ACC-CODE    =  38 )                               
"          AND ( LNP-LN-CODE   NOT   =   03      )                      
"          AND ( LNP-LN-CODE   NOT   =   55      )                      
"          AND ( LNP-LN-CODE   NOT   =   13      )                      
"          AND ( LNP-LN-CODE   NOT   =   67      )                      
"          THEN                                                         
981119       MOVE      '3'             TO      W-LIMIT-RTRN-CASE.       
981119     IF  (( LNP-RTRN-CODE   = 1 OR 2 OR 3 OR 4  )                 
"          OR  ( LNP-RTRN-CODE  = 9 ) AND ( LNP-RTRN-INTVL = 01 OR 02 ))
"          AND ( CTF-RECV-TYPE   = '1' OR '4' OR '7' )                  
"          AND  (TWA-C-SUP-OFF)                                         
"          THEN  IF ( W-LIMIT-RTRN-CASE = '1'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制一年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
981119     ELSE  IF ( W-LIMIT-RTRN-CASE = '2'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制二年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
981119     ELSE  IF ( W-LIMIT-RTRN-CASE = '3'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制三年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
981119     IF  (( LNP-RTRN-CODE   = 0 )                                 
"          OR  ( LNP-RTRN-CODE  = 9 ) AND ( LNP-RTRN-INTVL = 00 ))      
"          AND  (TWA-C-SUP-OFF)                                         
"          THEN  IF ( W-LIMIT-RTRN-CASE = '1'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制一年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
981119     ELSE  IF ( W-LIMIT-RTRN-CASE = '2'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制二年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
981119     ELSE  IF ( W-LIMIT-RTRN-CASE = '3'  )                        
"                THEN                                                   
"            MOVE        SPACES               TO   MSG-T282             
"            MOVE '此貸款在限制三年清償期間，須收取違約金。'          
"                                             TO   MSG-T282-LINE1       
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
040107*---------------〔８１修屋專案〕設定基本參數                    
"          IF        ( LNP-CAPTL-PURPOSE  =  '54' )                     
"          THEN                                                         
"                      PERFORM 302-CP54-SETUP-RTN     THRU  302-EXIT.   
"     *    ENDIF                                                        
050122*---------------交叉檢核：「科目／收回原因」                    
"          IF        ( LNL-ACC-CODE       =  '47' OR '48' OR '49' )     
"                AND ( CTF-CAPTL-PURPOSE  =  '81' )                     
"          THEN                                                         
"                      MOVE  SPACES              TO   MSG-T282          
"            MOVE '本案為「４７催收款項或４８呆帳」案件，'            
"                                                TO   MSG-T282-LINE1    
"            MOVE '「收回原因」不可登打【８１轉列催收】，'            
"                                                TO   MSG-T282-LINE2    
"            MOVE '請查明確認後重新登打！'     TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"     *    ENDIF                                                        
050614*---------------網銀清償：檢核〔短墊款〕是否已清                
"          IF        ( TWA-BR-CODE    =  '026' )                        
"                AND ( CTF-RECV-TYPE  =  '1' )                          
"          THEN                                                         
"                      PERFORM 361-AC-SHORT-CHK-RTN   THRU  361-EXIT.   
"     *    ENDIF                                                        
971222*---------------讀取戶名                                        
"          PERFORM     390-READ-CFNAME-RTN      THRU  390-EXIT.         
991203     PERFORM     399-READ-ACIF-RTN        THRU  399-EXIT.         
090224*---就學貸款其他應付款－沖本金以免掛帳太多                      
090224     IF       (  TWA-C-TXN-NOT-REVERSE )                          
           THEN                                                         
               IF   (  LNP-C-ENT-HGHER-SCH-LN )                         
                AND (  LNL-ACC-CODE =  37  )                            
                AND (  TWA-BR-CODE  =  101 )                            
                AND (  CTF-INT-AMT  =  +0  )                            
                AND (  CTF-FINE     =  +0  )                            
                AND (  LOG-AMT  NOT >  100 )                            
               THEN                                                     
                       MOVE     'Y'   TO SCH-RTNABLE                    
                       MOVE     CTF-RECV-TYPE  TO    W-CTF-RECV-TYPE    
                       PERFORM  9143-LN-ESTB-LNR-RTN                    
                       PERFORM  4902-0-AR-UPDATE-NML THRU 4902-0-EXIT   
                       PERFORM  4907-PREPARE-AFT     THRU 4907-EXIT     
                       GO   TO  300-EXIT.                               
090224     IF       (  TWA-C-TXN-REVERSE )                              
           THEN                                                         
               IF   (  LNP-C-ENT-HGHER-SCH-LN )                         
                AND (  LNL-ACC-CODE =  37  )                            
                AND (  TWA-BR-CODE  =  101 )                            
                AND (  CTF-INT-AMT  =  +0  )                            
                AND (  CTF-FINE     =  +0  )                            
                AND (  LOG-AMT  NOT >  100 )                            
               THEN                                                     
                       MOVE     'Y'   TO SCH-RTNABLE                    
                       MOVE     CTF-RECV-TYPE  TO    W-CTF-RECV-TYPE    
                       PERFORM 4909-RTRN-RVS    THRU    4909-EXIT       
                       PERFORM 4910-UPDATE-RVS  THRU    4910-EXIT       
                       GO   TO  300-EXIT.                               
       300-EXIT.
           EXIT.
------*---------------擷取LNP(先抓總號的資料)-----------------------
       300-LN-DB-P2-RTN.
           IF          CTF-APRV-SER-SUB-NO   NOT  =  0
           THEN
             MOVE      CTF-APRV-NO           TO   WS-CTF-APRV-NO
             MOVE      0                     TO   WS-CTF-APRV-SER-SUB-NO
             PERFORM   9026-LN-DBS-P2TT-RTN
             PERFORM   9105-LN-DBS-ERR-HDL-RTN
             MOVE      LNP                   TO   WLNP
             MOVE      LNP-APRV-AMT          TO   W-LNP-APRV-AMT
             MOVE      LNP-APRV-UNUSED-BAL   TO   W-LNP-APRV-UNUSED-BAL
             MOVE      LNP-CURRENCY-CODE     TO   W-CURRENCY-CODE.
      *---------------讀本案ＬＮＰ                                    
           PERFORM     9026-LN-DBS-P2-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           MOVE        LNP                   TO   CLNP.
           IF          CTF-APRV-SER-SUB-NO    =  0
           THEN
             MOVE      LNP-APRV-AMT          TO   W-LNP-APRV-AMT
             MOVE      LNP-APRV-UNUSED-BAL   TO   W-LNP-APRV-UNUSED-BAL
             MOVE      LNP-CURRENCY-CODE     TO   W-CURRENCY-CODE.
           MOVE        LNP-APRV-INTVL-CODE TO     AFT-APRV-INTVL-CODE.
           MOVE        LNP-APRV-INTVL      TO     AFT-APRV-INTVL.
970922     MOVE        LNP-APRV-DATE-R     TO     AFT-APRV-HI-DATE.     
           MOVE        LNP-APRV-NO         TO     W-LNP-APRV-NO.
840801     MOVE        W-LNP-APRV-SER-NO   TO     C-LNP-APRV-NO.        
           MOVE        LNP-FIN-ACC-SER-NO  TO     W-ACC-FIN-ACC-SER-NO.
           MOVE        LNP-FIN-PROP        TO     W-LNP-FIN-PROP.
           MOVE        LNP-LN-CODE         TO     AFT-LN-CODE.
           MOVE        LNP-STOP-CODE       TO     WS-LNP-STOP-CODE.
           MOVE        CTF-RECV-AMT    TO   W-LNL-LN-BAL W-CTF-AMT.     
       300-P2-EXIT.
              EXIT.                                                     
------*---------------〔８１修屋專案〕工作區設定----------------------
040107 302-CP54-SETUP-RTN.                                              
"     *---------------設定：第１年／第４年截止日                      
"          MOVE        LNL-1ST-LN-DATE      TO   WK54-1YY-END-DATE      
"                                                WK54-4YY-END-DATE.     
"          ADD         1                    TO   WK54-1YY-END-YY.       
"          ADD         4                    TO   WK54-4YY-END-YY.       
040107*---------------設定：補貼最高利率／現行實際利率                
"          MOVE        LNL-INT-RATE         TO   WK54-OLD-RATE.         
"          MOVE        ZEROS                TO   WK54-GOV-RATE.         
"          MOVE        LNL-INT-RATE         TO   WK54-NEW-RATE.         
"          IF        ( LNL-L-RECV-INT-DATE  NOT <  WK54-4YY-END-DATE )  
"                      MOVE    ZEROS             TO   WK54-GOV-RATE     
"                      MOVE    WK54-OLD-RATE     TO   WK54-NEW-RATE     
040107*---------------第２∼４年最高補貼１•５％                      
"          ELSE IF   ( LNL-L-RECV-INT-DATE  NOT <  WK54-1YY-END-DATE )  
"                AND ( WK54-OLD-RATE        NOT >  1.500 )              
"                      MOVE    WK54-OLD-RATE     TO   WK54-GOV-RATE     
"                      MOVE    ZEROS             TO   WK54-NEW-RATE     
"          ELSE IF   ( LNL-L-RECV-INT-DATE  NOT <  WK54-1YY-END-DATE )  
"                      MOVE    1.500             TO   WK54-GOV-RATE     
"                      COMPUTE WK54-NEW-RATE =   WK54-OLD-RATE          
"                                              - WK54-GOV-RATE          
040107*---------------第１年最高補貼２％                              
"          ELSE IF   ( WK54-OLD-RATE  NOT >  2.000 )                    
"                      MOVE    WK54-OLD-RATE     TO   WK54-GOV-RATE     
"                      MOVE    ZEROS             TO   WK54-NEW-RATE     
"          ELSE                                                         
"                      MOVE    2.000             TO   WK54-GOV-RATE     
"                      COMPUTE WK54-NEW-RATE =   WK54-OLD-RATE          
"                                              - WK54-GOV-RATE.         
"     *    ENDIF                                                        
"      302-EXIT.                                                        
"          EXIT.                                                        
------*---------------讀《限制清償設定檔LNFLMTR(6317)》-------------
141124 305-LMTR-READ-RTN.                                               
"          MOVE     LNM-ACC-NO               TO   LMTR-ACC-NO.          
"          MOVE     LNP-APRV-NO              TO   LMTR-APRV-NO.         
"          MOVE        LNL-1ST-LN-DATE      TO   WLMTR-END-DATE.        
"     *---------------讀檔                                            
"          MOVE        SPACES               TO   WLMTR-READ-MARK.       
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      NOTFND (305-NOTFND-END)          
"                                      DSIDERR(910-FILE-DSIDERR)        
"                                      INVREQ (910-FILE-INVREQ)         
"                                      ERROR  (910-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
"          MOVE   '（LNFLMTR限制清償設定檔)' TO   MSG-FILE-LINE2.   
141124     EXEC        CICS  READ      DATASET('LNFLMTR ')              
"                                      INTO   (LNFLMTR)                 
"                                      LENGTH (LMTR-LENGTH)             
"                                      RIDFLD (LMTR-KEY)                
"                                      UPDATE                           
"                      END-EXEC.                                        
141124*---------------有資料：計算截止日期                            
"          MOVE        'Y'                  TO   WLMTR-READ-MARK.       
"          PERFORM     305A-LMTR-END-DATE-RTN    THRU  305A-EXIT        
"                                 VARYING   II   FROM  1  BY  1         
"                                   UNTIL   II > 3.                     
BBBB  *---------------檢核顯示                                        
"          IF        ( DISP-MARK = 'Y' )                                
"          THEN                                                         
"             MOVE  'ＬＭＴＲ：[************]' TO   DISP1-G           
"             MOVE     LMTR-ACC-NO               TO   DISP1-G(14:12)    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP1-G)       END-EXEC    
"             MOVE  '　核准號碼[***********]'  TO   DISP1-G           
"             MOVE     LMTR-APRV-NO              TO   DISP1-G(14:11)    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP1-G)       END-EXEC    
"             MOVE  '　　　期間[***-***-***]'  TO   DISP1-G           
"             MOVE     LMTR-LMT-MONS(1)          TO   DISP1-G(14:03)    
"             MOVE     LMTR-LMT-MONS(2)          TO   DISP1-G(18:03)    
"             MOVE     LMTR-LMT-MONS(3)          TO   DISP1-G(22:03)    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP1-G)       END-EXEC    
"             MOVE  '　　截止日[*******]'      TO   DISP1-G           
"             MOVE     WLMTR-END-DATE            TO   DISP1-G(14:07)    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP1-G)       END-EXEC    
"             MOVE  '　本案初貸[*******-****]' TO   DISP1-G           
"             MOVE     LNL-1ST-LN-DATE           TO   DISP1-G(14:07)    
"             MOVE     LNL-LN-SER-NO             TO   DISP1-G(22:04)    
"             EXEC CICS WRITE OPERATOR  TEXT(DISP1-G)       END-EXEC    
"          END-IF.                                                      
141124*---------------正常結束                                        
"          GO    TO    305-EXIT.                                        
141124*---------------ＬＭＴＲ檔無資料                                
"      305-NOTFND-END.                                                  
"          MOVE        'N'             TO   WLMTR-READ-MARK.            
"          MOVE        ZEROS           TO   WLMTR-END-DATE.             
"      305-EXIT.                                                        
"          EXIT.                                                        
------*---------------計算《限制清償設定檔LNFLMTR(6317)》截止日-----
141124 305A-LMTR-END-DATE-RTN.                                          
"          IF        ( LMTR-LMT-MONS(II) NUMERIC )                      
"                AND ( LMTR-LMT-MONS(II) > ZEROS )                      
"          THEN                                                         
"                      NEXT    SENTENCE                                 
"          ELSE                                                         
"                      GO  TO  305A-EXIT                                
"          END-IF.                                                      
"          COMPUTE     WK-WORK1-NO =  WLMTR-END-MM + LMTR-LMT-MONS(II). 
"          DIVIDE      12            INTO   WK-WORK1-NO                 
"                                  GIVING   WK-WORK2-NO                 
"                               REMAINDER   WK-WORK3-NO.                
"          IF        ( WK-WORK2-NO = ZEROS )                            
"          THEN                                                         
"                      MOVE    WK-WORK3-NO       TO   WLMTR-END-MM      
"          ELSE                                                         
"                      ADD     WK-WORK2-NO       TO   WLMTR-END-YY      
"                      ADD     WK-WORK3-NO       TO   WLMTR-END-MM      
"          END-IF                                                       
"          MOVE        WLMTR-END-DATE       TO   WLMTR-LMT-DATE(II).    
"      305A-EXIT.                                                       
"           EXIT.                                                       
------*----清償檔檢核--------------------------------------           
930730 310-LNCLOSE-CHECK-RTN.                                           
"     *---------------限勞工住宅建購修繕貸款催收（非利息部份）使用    
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
040317           AND ( LNP-LN-CODE      =  '33' OR '34' )               
930730           AND ( LNL-ACC-CODE     =  '47' )                       
"                AND ( LNL-BC-COLM  NOT =  '1' )                        
"          THEN                                                         
"                      NEXT    SENTENCE                                 
"          ELSE                                                         
"                      GO  TO  310-EXIT.                                
"          MOVE        SPACES               TO   LNCLOSE.               
"          MOVE        CTF-ACC-NO           TO   CLS-ACC-NO.            
"          MOVE        CTF-LN-ID-NO         TO   CLS-LN-ID-NO.          
060412     MOVE   '（LNCLOSE法拍資料檔／６３２１交易）'             
"                                           TO   MSG-FILE-LINE2.        
930730     EXEC        CICS    HANDLE  CONDITION                        
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      NOTFND (310-NOTFND)              
"                                      ERROR  (910-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
"          EXEC        CICS    READ    DATASET('LNCLOSE')               
"                                      INTO   (LNCLOSE)                 
"                                      LENGTH (CLS-LENGTH)              
"                                      RIDFLD (CLS-KEY)                 
"                      END-EXEC.                                        
"          GO    TO    310-EXIT.                                        
"      310-NOTFND.                                                      
"          MOVE        SPACES               TO   MSG-T282.              
"          MOVE   '處分押品收回（收回類別１、收回原因１１），'        
"                                                TO   MSG-T282-LINE1.   
"          MOVE   '必須先鍵「６３２１放款法拍資料登錄」，'            
"                                                TO   MSG-T282-LINE2.   
"          MOVE   '請確認補登錄後，再重新輸入！' TO MSG-T282-LINE3.   
"          PERFORM     9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"      310-EXIT.                                                        
"          EXIT.                                                        
------*---------------就學貸款短期墊款檢查----------------------------
950912 320-ACLNID-CHECK-RTN.                                            
           IF          TWA-C-TXN-REVERSE                                
           THEN                                                         
                       GO       TO   320-EXIT.                          
"          EXEC        CICS    HANDLE  CONDITION                        
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      NOTFND (320-NOTFND)              
"                                      ERROR  (910-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
060421     MOVE   '（ACLNID學貸墊款資料檔）' TO   MSG-FILE-LINE2.   
           MOVE        LNM-CUST-ID          TO   ACID-NO.               
           EXEC CICS   READ                                             
                       DATASET('ACLNID')                                
                       INTO(ACLN-ID)                                    
                       RIDFLD(ACID-NO)                                  
                       LENGTH(ACLNID-LEN)                               
           END-EXEC.                                                    
970222     IF          ACID-SHORT-PAY       =    '1'                    
           THEN                                                         
"                      MOVE   SPACES        TO   MSG-T282               
B00101       MOVE '尚有短期墊款，請洽學貸科，劃付報單消除短'          
"                                                TO   MSG-T282-LINE1    
"            MOVE '期墊款後，始可收放款本息。' TO   MSG-T282-LINE2    
"                 PERFORM 9146A-CHINESE-MSG-RTN    THRU  9146A-EXIT.    
990707     IF          ACID-66INT-RCV       =    '1'                    
           THEN                                                         
"                      MOVE   SPACES        TO   MSG-T282               
B00101       MOVE '尚有應收利息，請洽學貸科，劃付報單消除應'          
"                                                TO   MSG-T282-LINE1    
"            MOVE '收利息後，始可收放款本息。' TO   MSG-T282-LINE2    
"                 PERFORM 9146A-CHINESE-MSG-RTN    THRU  9146A-EXIT.    
"      320-NOTFND.                                                      
"          EXEC        CICS    HANDLE  CONDITION                        
"                                      NOTFND (910-FILE-NOTFND)         
"                      END-EXEC.                                        
"      320-EXIT.                                                        
"          EXIT.                                                        
------*---------------就學貸款信保基金理賠----------------------------
961226 3210-TRUST-AMT-RTN.                                              
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN(910-NOT-OPEN)                            
"                      ERROR(910-OTHER-ERROR)                           
"                      END-EXEC.                                        
           MOVE        CTF-ACC-NO           TO   E04-ACC-NO.            
           MOVE        CTF-APRV-NO          TO   E04-APRV-NO.           
           EXEC  CICS  READ  DATASET('SCHED04')                         
                             INTO(E04-DATA)                             
                             LENGTH(ED04-LEN)                           
                             RIDFLD(E04-KEY)                            
                       END-EXEC.                                        
           MOVE        CTF-RECV-AMT    TO  W-AMT-10.                    
           IF          LNL-BC-COLM  =  '1'                              
                       MOVE  1         TO   W-TYPE                      
           ELSE IF     LNL-BC-COLM  =  '2'                              
                       MOVE  2         TO   W-TYPE                      
           ELSE                                                         
                       MOVE  0         TO   W-TYPE                      
           END-IF.                                                      
           COMPUTE     W-AMT-8 ROUNDED      =    W-AMT-10      * 0.8.   
      *-------避免有基金未還餘額                                      
           IF          (TWA-C-TXN-NOT-REVERSE)                          
           THEN                                                         
               IF      CTF-RECV-AMT  =  LNL-LN-BAL                      
               AND   ( W-TYPE        =  0 OR 2 )                        
               THEN                                                     
                       MOVE  LNL-INSUR-AMT TO    W-AMT-8.               
      *-------避免有基金未還餘額（利息處理）                          
           IF          (TWA-C-TXN-NOT-REVERSE)                          
           THEN                                                         
               IF      W-TYPE          =    1                           
               THEN                                                     
                    IF ( W-AMT-8    NOT  <    LNL-INSUR-AMT )           
                    THEN                                                
                       MOVE  LNL-INSUR-AMT TO    W-AMT-8.               
           COMPUTE     W-AMT-2         =    W-AMT-10     - W-AMT-8.     
           COMPUTE     LNL-INSUR-AMT   =    LNL-INSUR-AMT  - W-AMT-8.   
           IF          (TWA-C-TXN-NOT-REVERSE)                          
           THEN                                                         
               IF      CTF-RECV-AMT         >    LNL-LN-BAL             
               THEN                                                     
"                      MOVE  SPACES              TO   MSG-T282          
"            MOVE '「信保基金不可超過餘額」'   TO   MSG-T282-LINE1    
"            MOVE '重新輸入本金！'             TO   MSG-T282-LINE2    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
       3210-EXIT.                                                       
           EXIT.                                                        
120710*---------------就學貸款檢查是否為疫後補貼戶--------------------
120710 330-SCHSUDY-PROC-RTN.                                            
"     *---------------沖正交易無須控管                                
"          IF          TWA-C-TXN-REVERSE                                
"          THEN                                                         
"                      GO       TO   330-EXIT                           
"          END-IF.                                                      
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTFND  (330-EXIT)                               
"                      ERROR   (330-EXIT)                               
"                      NOTOPEN (330-EXIT)                               
"                      END-EXEC.                                        
"          MOVE        LNM-ACC-NO             TO   SHSDY-ACC-NO.        
"          MOVE   '（SCHSUDY學貸疫後補貼資料檔）' TO MSG-FILE-LINE2.
"          EXEC CICS   READ  DATASET('SCHSUDY ')                        
"                            INTO(SHSDY-REC)                            
"                            RIDFLD(SHSDY-ACC-NO)                       
"                            LENGTH(SCHSUDY-LEN)                        
"          END-EXEC.                                                    
"     *---------------檔案狀態正常，表示補助款未執行完畢，不可繳款    
"          IF        ( SHSDY-PROC-STATUS = '0' OR '1' OR SPACES )       
"          THEN                                                         
"                      MOVE   SPACES        TO   MSG-T282               
"                      MOVE '尚有教育部疫後補助款，尚未沖償完畢，'    
"                                                TO   MSG-T282-LINE1    
"                      MOVE '請洽學貸科。'     TO   MSG-T282-LINE2    
"                      PERFORM  9146A-CHINESE-MSG-RTN  THRU  9146A-EXIT 
"          END-IF.                                                      
120710 330-EXIT.                                                        
120710     EXIT.                                                        
------*---------------特別協議：部分本息協議分期攤還不收逾期息--------
981125 358-LN24-CHK.                                                    
           EXEC CICS   HANDLE CONDITION                                 
"                      NOTFND (358-EXIT)                                
"                      ERROR  (358-EXIT)                                
"                      ENDFILE(358-EXIT)                                
"                      END-EXEC.                                        
           MOVE        CTF-ACC-NO          TO     LNF24-ACC-NO.         
           MOVE        CTF-LN-ID-NO        TO     LNF24-LN-ID-NO.       
           EXEC  CICS  READ   DATASET('ALNGVRF')                        
                              INTO(ALNF24-REC)                          
                              LENGTH(F24-LENGTH)                        
                              RIDFLD(LNF24-KEY)                         
           END-EXEC.                                                    
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
                 AND ( LNF24-FILE-STATUS  =  9 )                        
           THEN                                                         
                       GO    TO   358-EXIT.                             
"          IF        ( TWA-C-TXN-REVERSE )                              
           THEN                                                         
                       MOVE  LNF24-REV-STATUS    TO  LNF24-FILE-STATUS. 
           IF        ( LNF24-FILE-STATUS  =  0 OR 1 )                   
           THEN                                                         
                       MOVE    'Y'          TO    F24-MARK.             
           IF          LNF24-FILE-STATUS   =      2                     
           THEN                                                         
                       MOVE    'C'          TO    F24-MARK              
                       MOVE     LNF24-AFTER-EACH  TO W-AMT-8            
                       GO    TO   358-EXIT.                             
           IF   NOT    CTF-RECV-TYPE   =  2                             
           THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"                      MOVE 'M693A公教協議攤還限定使用ＴＹＰＥ：２'   
"                                           TO   MSG-T282-LINE1         
                       PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
           THEN                                                         
                       ADD      1       TO  LNF24-ACUM-TERM.            
           IF          LNF24-TERM-NO    =   LNF24-ACUM-TERM             
           THEN                                                         
                       MOVE     LNF24-LAST-AMT TO W-AMT-10              
                       MOVE     LNF24-LAST-INT TO W-AMT-8               
                       MOVE     2              TO LNF24-FILE-STATUS     
           ELSE                                                         
                       MOVE     1              TO LNF24-FILE-STATUS     
                       MOVE     LNF24-EACH-AMT TO W-AMT-10              
                       MOVE     LNF24-EACH-INT TO W-AMT-8.              
"          MOVE        CTF-RECV-AMT         TO   WK-CHK-AMT.            
"          MOVE        CTF-INT-AMT          TO   W-AMT-2.               
           IF          WK-CHK-AMT    NOT    =    W-AMT-10               
                OR     W-AMT-2       NOT    =    W-AMT-8                
           THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M693A公教協議攤還金額錯誤請查明６６２交易 '        
"                                                TO   MSG-T282-LINE1    
             MOVE '確認金額後重新輸入'         TO   MSG-T282-LINE2    
                       PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
"      358-EXIT.                                                        
"          EXIT.                                                        
A10423 3581-LN28-CHK.                                                   
           EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN (3581-EXIT)                              
"                      NOTFND  (3581-EXIT)                              
"                      ERROR   (3581-EXIT)                              
"                      INVREQ  (3581-EXIT)                              
"                      ENDFILE (3581-EXIT)                              
"                      END-EXEC.                                        
           MOVE        CTF-ACC-NO          TO     LNF28-ACC-NO.         
           MOVE        CTF-LN-ID-NO        TO     LNF28-LN-ID-NO.       
           EXEC  CICS  READ                                             
                       DATASET('LNCTNRF ')                              
                       INTO(LNF28-REC)                                  
                       LENGTH(F28-LENGTH)                               
                       RIDFLD(LNF28-KEY)                                
           END-EXEC.                                                    
           IF          LNF28-FILE-STATUS   =      9                     
           THEN                                                         
                       GO    TO   3581-EXIT.                            
           IF        ( LNF28-FILE-STATUS  =  0 OR 1 )                   
           THEN                                                         
                       MOVE    'Y'     TO         F28-MARK.             
      *---------------中途攤還                                        
           IF        ( CTF-INQ-TYPE  =  '7' )                           
           THEN                                                         
                       MOVE  SPACES         TO   MSG-T282               
             MOVE 'M663A交易６６２１不得作TＴＹＰＥ：７　 '         
                                                 TO   MSG-T282-LINE1    
             MOVE '以單純收款作業'             TO   MSG-T282-LINE2    
                       PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
      *---------------重設上次繳息日：不可與繳息日同月份              
           MOVE        LNL-L-RECV-INT-DATE TO      WKS-L-RECV-INT-DATE.
           IF        ( WKS-L-RECV-INT-YY  =  LNL-RECV-INT-YY )          
                 AND ( WKS-L-RECV-INT-MM  =  LNL-RECV-INT-MM )          
           THEN                                                         
                       COMPUTE WKS-L-RECV-INT-MM =    WKS-L-RECV-INT-MM 
                                                   -  1                 
               IF    ( WKS-L-RECV-INT-MM  =  ZEROS )                    
               THEN                                                     
                       MOVE  12        TO   WKS-L-RECV-INT-MM           
                       COMPUTE WKS-L-RECV-INT-YY =    WKS-L-RECV-INT-YY 
                                                   -  1.                
      *---------------計算上次繳息日的月底日                          
040317     MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
100406     MOVE        01                   TO   WK-DAT-BEG-DATE(6:2).  
"          PERFORM     808-DAT-08-CAL-RTN        THRU  808-EXIT.        
"          MOVE        DAT-P-CORR-DATE      TO   WKS-END-DATE.          
040317*---------------設定上次繳息日的相對日                          
"          IF        ( WKS-L-RECV-INT-DD  NOT =   LNL-RECV-INT-DAY )    
"          THEN                                                         
"                      MOVE  LNL-RECV-INT-DAY    TO   WKS-L-RECV-INT-DD.
"     *    ENDIF                                                        
"          IF        ( WKS-L-RECV-INT-DATE  >  WKS-END-DATE )           
"          THEN                                                         
"                      MOVE    WKS-END-DD        TO   WKS-L-RECV-INT-DD.
"     *    ENDIF                                                        
040317*---------------計算本次攤還期數                                
"          MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
"          MOVE        CTF-INT-END-DATE-N   TO   WK-DAT-END-DATE.       
"          MOVE        LNL-RECV-INT-DAY     TO   WK-DAT-DAYS.           
"          MOVE        01                   TO   WK-DAT-TERM-UNIT.      
"          PERFORM     815-DAT-15-CAL-RTN        THRU  815-EXIT.        
"          MOVE        DAT-P-TERM-NO        TO   WKS-TERM-NO.           
"      3581-EXIT.                                                       
"          EXIT.                                                        
------*---------------計算：本次攤還期數------------------------------
040317 360-RTRN-TERM-CAL-RTN.                                           
"     *---------------重設上次繳息日：不可與繳息日同月份              
"          MOVE        LNL-L-RECV-INT-DATE  TO   WKS-L-RECV-INT-DATE.   
"          IF        ( WKS-L-RECV-INT-YY  =  LNL-RECV-INT-YY )          
"                AND ( WKS-L-RECV-INT-MM  =  LNL-RECV-INT-MM )          
"          THEN                                                         
"                      COMPUTE WKS-L-RECV-INT-MM =    WKS-L-RECV-INT-MM 
"                                                  -  1                 
"            IF      ( WKS-L-RECV-INT-MM  =  ZEROS )                    
"            THEN                                                       
"                      MOVE    12                TO   WKS-L-RECV-INT-MM 
"                      COMPUTE WKS-L-RECV-INT-YY =    WKS-L-RECV-INT-YY 
"                                                  -  1.                
"     *    ENDIF                                                        
040317*---------------計算上次繳息日的月底日                          
"          MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
100406     MOVE        01                   TO   WK-DAT-BEG-DATE(6:2).  
"          PERFORM     808-DAT-08-CAL-RTN        THRU  808-EXIT.        
"          MOVE        DAT-P-CORR-DATE      TO   WKS-END-DATE.          
040317*---------------設定上次繳息日的相對日                          
"          IF        ( WKS-L-RECV-INT-DD  NOT =  LNL-RECV-INT-DAY )     
"          THEN                                                         
"                      MOVE  LNL-RECV-INT-DAY    TO   WKS-L-RECV-INT-DD.
"     *    ENDIF                                                        
"          IF        ( WKS-L-RECV-INT-DATE  >  WKS-END-DATE )           
"          THEN                                                         
"                      MOVE    WKS-END-DD        TO   WKS-L-RECV-INT-DD.
"     *    ENDIF                                                        
040317*---------------設定：攤還期間                                  
"          IF        ( LNP-RTRN-INTVL  =  '01' )                        
"                      MOVE    01                TO   WK-DAT-TERM-UNIT  
"          ELSE IF   ( LNP-RTRN-INTVL  =  '02' )                        
"                      MOVE    06                TO   WK-DAT-TERM-UNIT  
"          ELSE                                                         
"                      MOVE    01                TO   WK-DAT-TERM-UNIT. 
"     *    ENDIF                                                        
040317*---------------計算期數：上次繳息日∼繳息迄日                  
"          MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
"          MOVE        CTF-INT-END-DATE-N   TO   WK-DAT-END-DATE.       
"          MOVE        LNL-RECV-INT-DAY     TO   WK-DAT-DAYS.           
"          PERFORM     815-DAT-15-CAL-RTN        THRU  815-EXIT.        
"          IF        ( WK-DAT-BEG-DATE  NOT <  WK-DAT-END-DATE )        
"          THEN                                                         
"                      MOVE    ZEROS             TO   DAT-P-TERM-NO     
"          ELSE                                                         
"                      PERFORM 815-DAT-15-CAL-RTN     THRU  815-EXIT.   
"     *    ENDIF                                                        
"          MOVE        DAT-P-TERM-NO        TO   WKS-RTRN-TERM-NO.      
"      360-EXIT.                                                        
"          EXIT.                                                        
------*--------------檢核〔短期墊款／１５５３１科目〕-----------------
050614 361-AC-SHORT-CHK-RTN.                                            
"     *---------------設定：西元交易日                                
"          COMPUTE     WK-WEST-YY =    TWA-TXN-YY7 + 1911.              
"          MOVE        TWA-TXN-MM7          TO   WK-WEST-MM.            
"          MOVE        TWA-TXN-DD7          TO   WK-WEST-DD.            
050614*---------------讀【ＡＣＬＮＣＲＦ短墊檔】：設定ＫＥＹ          
"     *---------------子目：０２００放款科                            
"          MOVE        '0200'               TO   ACLN-SUB.              
"          MOVE        LNM-ACC-NO           TO   ACLN-BUSN-ID.          
"          MOVE        WK-WEST-DATE         TO   ACLN-DATE.             
"          MOVE        ZEROS                TO   ACLN-SEQ-NO.           
"          PERFORM     362-ACLNCRF-PROC-RTN      THRU  362-EXIT.        
050614*---------------子目：０２０１代墊保險費                        
"          MOVE        '0201'               TO    ACLN-SUB.             
"          MOVE        LNM-ACC-NO           TO   ACLN-BUSN-ID.          
"          MOVE        WK-WEST-DATE         TO   ACLN-DATE.             
"          MOVE        ZEROS                TO   ACLN-SEQ-NO.           
"          PERFORM     362-ACLNCRF-PROC-RTN      THRU  362-EXIT.        
050614*---------------子目：０４００信用卡戶                          
"          MOVE        '0400'               TO    ACLN-SUB.             
"          MOVE        LNM-ACC-NO           TO   ACLN-BUSN-ID.          
"          MOVE        WK-WEST-DATE         TO   ACLN-DATE.             
"          MOVE        ZEROS                TO   ACLN-SEQ-NO.           
"          PERFORM     362-ACLNCRF-PROC-RTN      THRU  362-EXIT.        
050614*-----------讀【ＡＣＬＮＩＤ１國宅短墊檔】                      
"          IF        ( LNM-ACC-BUSINS-CODE  =  '69' )                   
"          THEN                                                         
"                      MOVE    LNM-ACC-NO        TO   LNID1-KEY         
"                      PERFORM 363-ACLNID1-PROC-RTN   THRU  363-EXIT.   
"     *    ENDIF                                                        
"      361-EXIT.                                                        
"          EXIT.                                                        
------*---------------讀會計短墊資料檔--------------------------------
050614 362-ACLNCRF-PROC-RTN.                                            
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTFND (362-EXIT)                
"                                      NOTOPEN(910-NOT-OPEN)            
"                                      ERROR  (910-OTHER-ERROR)         
"                      END-EXEC.                                        
050614     MOVE        +160                 TO   FILE-LENGTH.           
"          EXEC        CICS  READ DATASET('ACLNCRF ')                   
"                                 INTO   (ACLN-REC)                     
"                                 RIDFLD (ACLN-KEY)                     
"                                 LENGTH (FILE-LENGTH)                  
"          END-EXEC.                                                    
050614*---------------排除：餘額＝０                                  
"          IF        ( ACLN-AMT  =   ZEROS )                            
"          THEN                                                         
"                      GO  TO  362-EXIT.                                
"     *    ENDIF                                                        
050614*---------------交易結束（不可於網銀清償）                      
"          MOVE        'M6B4'               TO   MSG-P-OUT-CODE.        
"          PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
"      362-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
050614 363-ACLNID1-PROC-RTN.                                            
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTFND (363-EXIT)                
"                                      NOTOPEN(910-NOT-OPEN)            
"                                      ERROR  (910-OTHER-ERROR)         
"                      END-EXEC.                                        
050614     MOVE        +20                  TO   FILE-LENGTH.           
"          EXEC        CICS  READ DATASET('ACLNID1 ')                   
"                                 INTO   (LN-ID1-REC)                   
"                                 RIDFLD (LNID1-KEY)                    
"                                 LENGTH (FILE-LENGTH)                  
"          END-EXEC.                                                    
050614*---------------交易結束（不可於網銀清償）                      
"          MOVE        'M6B4'               TO   MSG-P-OUT-CODE.        
"          PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
"      363-EXIT.                                                        
"          EXIT.                                                        
------*---------------〔６轉籍移出〕警示帳戶禁止轉籍----------------- 
******* 380段－LNK380                                                 
131011   COPY LNK380.                                                   
------*---------------擷取客戶名稱------------------------------------
971210 390-READ-CFNAME-RTN.                                             
"          MOVE        SPACES          TO   WK-CUST-NAME.               
"          MOVE        X'0E'           TO   WK-NAME-0E.                 
"          MOVE        X'0F'           TO   WK-NAME-0F.                 
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTFND (390-EXIT)                
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      ERROR  (910-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
971210     IF        ( LNM-TAX-ID  NOT =  ZEROS AND SPACES )            
"          THEN                                                         
"                      MOVE  '00'           TO   WS-CFM-ID-A            
"                      MOVE  LNM-TAX-ID     TO   WS-CFM-ID-B            
"          ELSE                                                         
"                      MOVE  LNM-CUST-ID    TO   WS-CFM-ID.             
971210     MOVE        WS-CFM-ID            TO   CFM-ID-NO.             
990721     MOVE        LNM-CFM-SER          TO   CFM-SER.               
060421     MOVE   '（CFNAME客戶中文資料檔）' TO   MSG-FILE-LINE2.   
990721     EXEC        CICS  READ DATASET('CFNAME')                     
"                                 INTO   (CFMV)                         
"                                 LENGTH (CFM-LENGTH)                   
"                                 RIDFLD (CFM-KEY)                      
"                      END-EXEC.                                        
"          MOVE        CFM-CHINESE-NAME     TO   WK-CUST-NAME.          
"      390-EXIT.                                                        
           EXIT.                                                        
------*---------------讀ＡＣＩＦ日期錄--------------------------------
991203 399-READ-ACIF-RTN.                                               
"          MOVE        ZEROS               TO      ACIF-KEY.            
060421     MOVE   '（UCPACI7公用資訊檔）'    TO   MSG-FILE-LINE2.   
991203     EXEC    CICS    READ     DATASET('UCPACI7 ')                 
"                                   INTO(ACIF-D)                        
"                                   RIDFLD(ACIF-KEY)                    
"                                   LENGTH(ACIF-LEN)                    
"          END-EXEC.                                                    
------*----判斷在當月提存日前後　　　　　　　　　　　　　　           
"          IF   ( ACIF-D-N-N-BUSINS-MM = ACIF-D-TODAY-MM )              
"          THEN IF  ( CTF-INT-END-MM-N  < ACIF-D-TODAY-MM )             
"                 MOVE        'B'          TO      WK-ADJ-FLAG          
"               ELSE                                                    
"                 MOVE        'A'          TO      WK-ADJ-FLAG          
"          ELSE IF  ( CTF-INT-END-MM-N  > ACIF-D-TODAY-MM )             
"                 MOVE        'A'          TO      WK-ADJ-FLAG          
"               ELSE                                                    
"                 MOVE        'B'          TO      WK-ADJ-FLAG.         
"      399-EXIT.                                                        
991203     EXIT.                                                        
------*---------------檔案資料檢驗------------------------------------
       400-CHECK-PROC-RTN.
090224     IF          SCH-RTNABLE         =        'Y'                 
           THEN                                                         
                       GO  TO  400-EXIT.                                
050614*-----------------------------------------------------------------
"     *為利分行作業（預繳後再中途攤還，客戶利息多繳），取消此控管     
"     *-----------------------------------------------------------------
881018     IF        ( LNP-CAPTL-PURPOSE  =  '43' OR '44' )             
"          THEN                                                         
"                      MOVE      'M674'      TO      MSG-P-OUT-CODE     
"                      PERFORM   9146-LN-ERR-MSG-OUT-RTN.               
960521     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"                AND ( CTF-RECV-TYPE  NOT =  2 )                        
"          THEN                                                         
"                      MOVE  'M686'  TO  MSG-P-OUT-CODE                 
"                      PERFORM   9146-LN-ERR-MSG-OUT-RTN.               
120105*---------------控管呆帳僅可沖償到０元                          
960522     IF        ( LNL-ACC-CODE  =  '48' OR '49' )                  
961226          AND  ( TWA-C-TXN-NOT-REVERSE )                          
"               AND  ( CTF-RECV-AMT  >  LNL-LN-BAL )                    
"          THEN                                                         
"                MOVE      'M679'              TO      MSG-P-OUT-CODE   
"                PERFORM   9146-LN-ERR-MSG-OUT-RTN.                     
      *-----------------控管４４科目不可執行現金類收息                
141020     IF   (   TWA-TXN-ID-CODE         =  6710 OR 6722             
                                           OR  6740 OR 6752  )          
           AND  (   LNL-ACC-CODE            =  44            )          
           THEN                                                         
               MOVE  SPACES                TO  MSG-T282                 
               MOVE  'E044A科目４４應收代放款，不可執行現金收本'      
                                           TO  MSG-T282-LINE1           
               MOVE  '息，請改以轉帳收本息處理。'  TO  MSG-T282-LINE2 
               PERFORM 9146A-CHINESE-MSG-RTN   THRU 9146A-EXIT          
           END-IF.                                                      
920617*---------------應收代放款限制只有營業部能收                    
900413     IF        ( LNL-ACC-CODE  =  44 )                            
"                AND ( TWA-BR-CODE  NOT =  101 )                        
"          THEN                                                         
"            MOVE      'M674'              TO      MSG-P-OUT-CODE       
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
"          IF        ( LNL-ACC-CODE  =  '47' OR '57' )                  
"              AND                                                      
"                   (( CTF-INT-AMT  >  ZEROS )                          
"                 OR ( CTF-FINE     >  ZEROS ))                         
"          THEN                                                         
990810       IF      ( CTF-RECV-TYPE      =  '1' )                      
"                AND ( LNL-BC-COLM    NOT =  '1' )                      
"            THEN                                                       
"                      NEXT    SENTENCE                                 
"            ELSE                                                       
990810                 MOVE    SPACES       TO   MSG-T282               
"            MOVE 'M660A依規定「催收款項」停止計息，催收期間產生之'   
"                                                TO   MSG-T282-LINE1    
"            MOVE '利息，限非利息轉列之催收（ＢＣ欄非１）且為結案'    
"                                                TO   MSG-T282-LINE2    
"            MOVE '收回（類別＝１）交易，才可鍵入，請查明確認！'      
"                                                TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
      *---------------〔學貸〕轉換：收回類別                          
890209     IF          LNM-C-ACC-LN-SCHOOL                              
"                AND   LNP-C-ENT-HGHER-SCH-LN                           
                 AND   (LNL-LN-BAL     NOT >        LNL-EACH-RTRN-AMT)
890522           AND  (CTF-RECV-TYPE NOT = 5)                           
030123           AND ( LNL-ACC-CODE  =  '37' )                          
           THEN
             MOVE      1                   TO       CTF-RECV-TYPE.
890209     IF          LNM-C-ACC-LN-SCHOOL                              
"                AND   LNP-C-ENT-HGHER-SCH-LN                           
900706           AND   (CTF-INT-END-DATE-N-R NOT <    LNL-DUE-DATE-R)   
890522           AND  (CTF-RECV-TYPE NOT = 5)                           
030123           AND ( LNL-ACC-CODE  =  '37' )                          
           THEN
             MOVE      1                   TO       CTF-RECV-TYPE.
                                                                        
           MOVE        CTF-RECV-TYPE       TO       W-CTF-RECV-TYPE.
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
             IF          ((CTF-C-RTRN)
                    AND  (CTF-RECV-AMT  NOT = LNL-LN-BAL))
             THEN
               MOVE      'M679'              TO      MSG-P-OUT-CODE
               PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
           IF           (CTF-C-RECV)                                    
                   AND  (CTF-RECV-AMT = LNL-LN-BAL)                     
             THEN                                                       
060824            IF  ( LNL-ACC-CODE = 48 OR 49 OR '58' )               
                  THEN                                                  
                         NEXT  SENTENCE                                 
                  ELSE                                                  
                         MOVE  'M679'     TO      MSG-P-OUT-CODE        
                         PERFORM   9146-LN-ERR-MSG-OUT-RTN.             
890922     IF          (W-CTF-C-MID-RTRN)                               
           THEN                                                         
890922       IF        (LNL-C-RECVABLE-INT-CODE )                       
  "                OR  (LNL-C-LN-RC-FOR-OTHER)                          
  "          THEN                                                       
                   NEXT   SENTENCE                                      
             ELSE                                                       
  "            MOVE    'M083'              TO      MSG-P-OUT-CODE       
  "            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                       
890922     IF          (LNL-C-AR-OVERDUE                                
  "                OR   LNP-C-ACCUM)                                    
   "         AND        (W-CTF-C-MID-RTRN)                              
  "        THEN                                                         
  "          MOVE      'M659'              TO      MSG-P-OUT-CODE       
  "          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
890922     IF           (LNP-C-INT-FREE)                                
   "         AND        (W-CTF-C-MID-RTRN)                              
           THEN                                                         
  "          MOVE      'M659'              TO      MSG-P-OUT-CODE       
  "          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
890922     IF          (TWA-C-TXN-NOT-REVERSE)                          
   "       THEN                                                         
   "         IF        ((W-CTF-C-MID-RTRN)                              
   "               AND  (CTF-RECV-AMT NOT < LNL-LN-BAL))                
   "         THEN                                                       
   "         MOVE      'M679'              TO      MSG-P-OUT-CODE       
   "         PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
      *---------------設定攤還方式（WKS-RTRN-TYPE）                 
           MOVE        LNP-RTRN-TYPE       TO      WKS-RTRN-TYPE.       
           IF          LNP-C-TERM-AVER-RTRN
                AND    LNP-C-RTRN-NORMAL
           THEN
             MOVE      0                   TO      WKS-RTRN-TYPE.
           IF          LNP-C-TERM-AVER-RTRN
                AND    LNP-C-RTRN-MONTH-1
                AND    LNP-C-TERM-EACH-MONS
           THEN
             MOVE      1                   TO      WKS-RTRN-TYPE.
           IF          LNP-C-TERM-AVER-RTRN
                AND    LNP-C-RTRN-MONTH-1
                AND    LNP-C-TERM-ANN-EACH-MONS
           THEN
             MOVE      2                   TO      WKS-RTRN-TYPE.
040317     MOVE        WKS-RTRN-TYPE        TO   WKS-REAL-RTRN-CODE.    
      *---------------催收、採積數計息                                
040317     IF        ( LNL-C-AR-OVERDUE  OR  LNP-C-ACCUM )              
           THEN
             PERFORM   410-0-AR-OVERDUE-PROC       THRU    410-0-EXIT
             GO  TO    400-EXIT.
      *---------------免息                                            
           IF           (LNP-C-INT-FREE)
           THEN
             PERFORM   410-1-INT-FREE-PROC THRU    410-1-EXIT
             GO  TO    400-EXIT.
      *---------------中途攤還７：檢核、將攤還類別轉成一般處理        
890922     IF          W-CTF-C-MID-RTRN                                 
           THEN                                                         
950301       IF      ( WKS-RTRN-TYPE  NOT =  '2' )                      
             THEN                                                       
               MOVE    'M647'              TO      MSG-P-OUT-CODE       
               PERFORM 9146-LN-ERR-MSG-OUT-RTN                          
             ELSE                                                       
901109         MOVE    WKS-RTRN-TYPE       TO      BEF-RTRN-STYLE       
940506         MOVE    LNP-RTRN-CODE       TO      WK7-LNP-RTRN-CODE    
"              MOVE    WKS-RTRN-TYPE       TO      WK7-WKS-RTRN-CODE    
               MOVE    0                   TO      WKS-RTRN-TYPE.       
040324     IF        ( LNM-C-ACC-LN-NATION       )                      
                AND  ( LNP-CT-INTVL    =    '01' )                      
  "        THEN                                                         
                       MOVE    LNP-RTRN-CODE   TO  BEF-RTRN-STYLE       
                       MOVE    LNP-RTRN-CODE   TO  BEF-RTRN-CODE        
                       MOVE    LNL-EACH-RTRN-AMT TO BEF-EACH-RTRN-AMT   
                       MOVE    ZEROS           TO  WKS-RTRN-TYPE        
                       MOVE    ZEROS       TO      LNL-EACH-RTRN-AMT    
                       MOVE    ZEROS       TO      LNP-RTRN-CODE.       
      *    ENDIF                                                        
      *---------------收回１將攤還類別轉成一般處理                    
891011     IF          W-CTF-C-RTRN                                     
891017     AND  NOT   (LNM-C-ACC-LN-SCHOOL                              
"                AND   LNP-C-ENT-HGHER-SCH-LN)                          
           THEN                                                         
901109         MOVE    WKS-RTRN-TYPE       TO      BEF-RTRN-STYLE       
               MOVE    0                   TO      WKS-RTRN-TYPE.       
891011     IF          W-CTF-C-RTRN                                     
990125          AND  ( LNP-C-SCH-GEN )                                  
           THEN                                                         
901109         MOVE    WKS-RTRN-TYPE       TO      BEF-RTRN-STYLE       
               MOVE    0                   TO      WKS-RTRN-TYPE.       
040317*---------------設定：現行攤還方式                              
"          IF        ( LNP-RTRN-CODE   =  '9' )                         
"                AND ( LNP-RTRN-INTVL  =  ZEROS )                       
"                      MOVE    ZEROS             TO   WK7-WKS-RTRN-CODE 
"          ELSE IF   ( LNP-RTRN-CODE   =  '9' )                         
"                      MOVE  LNP-AFTER-TERM-RTRN-TYPE                   
"                                                TO   WK7-WKS-RTRN-CODE 
"          ELSE                                                         
"                      MOVE    LNP-RTRN-CODE     TO   WK7-WKS-RTRN-CODE.
"     *    ENDIF                                                        
901109*---------------失業勞工利息展延：計算應攤還利息                
901109     MOVE        ZEROS                TO   WK-LABOR-DELAY-INT.    
901109     PERFORM     560-LBI-INT-HDL-RTN  THRU  560-EXIT.             
981125*---------------公教寬繳利息展延：計算應攤還利息                
981125     IF          F24-MARK   = 'C'                                 
           THEN                                                         
981125                 MOVE    ZEROS       TO   W-AMT-2                 
                       PERFORM 570-F24-INT-HDL-RTN THRU 570-EXIT.       
110628*---------------青年創業啟動金補貼案件須一期一期繳費            
"     *---------------以利計算補貼資料                                
"          IF        ( LNL-ACC-CODE  =  '23' OR '24' OR '25' OR '26'    
"                                   OR  '37' OR '38' )                  
"              AND   ( LNP-LN-CODE   =  '69'         )                  
"              AND  (( LNL-1ST-LN-DATE-R  >  1090930 )                  
"               AND  ( LNL-1ST-LN-DATE-R  <  1110731 ))                 
"              AND  (( CTF-INT-END-DATE-N-R  >  LNL-RECV-INT-DATE-R )   
120710          AND  ( LNL-RECV-INT-DATE-R NOT = WKS-DUE-DATE       ))  
"              AND   ( W-CTF-RECV-TYPE  =  '1' OR '2' OR '4' OR '7' )   
"          THEN                                                         
"                      MOVE    SPACES            TO   MSG-T282          
"                      MOVE '本案為「青年創業啟動金」授信案件'        
"                                                TO   MSG-T282-LINE1    
"                      MOVE '請勿跨期繳費，以利補貼資料計算'          
"                                                TO   MSG-T282-LINE2    
"                      MOVE '請重新登打！'     TO   MSG-T282-LINE3    
"                      PERFORM 9146A-CHINESE-MSG-RTN  THRU  9146A-EXIT. 
110628*    ENDIF                                                        
      *---------------依收回類別分類處理                              
           PERFORM     410-CASE-BEG        THRU    410-CASE-END.
       400-EXIT.
           EXIT.
------*---------------檔案資料檢驗更新--------------------------------
       410-CASE-BEG.
           IF          (W-CTF-C-RTRN)      GO  TO  410-CASE-RTRN.
           IF          (W-CTF-C-RECV)      GO  TO  410-CASE-RECV.
           IF          (W-CTF-C-TR)        GO  TO  410-CASE-TR.
           IF          (W-CTF-C-INSTMENT)  GO  TO  410-CASE-INSTMENT.
           IF          (W-CTF-C-TR-OVER)   GO  TO  410-CASE-TR-OVER.
           IF          (W-CTF-C-MVOUT)     GO  TO  410-CASE-MVOUT.
890922     IF          (W-CTF-C-MID-RTRN)  GO  TO  410-CASE-MID-RTRN.   
           GO  TO      410-CASE-ERR.
       410-CASE-RTRN.
           PERFORM     411-CASE-BEG        THRU    411-CASE-END.
           GO  TO      410-CASE-END.
       410-CASE-RECV.
           PERFORM     412-CASE-BEG        THRU    412-CASE-END.
           GO  TO      410-CASE-END.
       410-CASE-TR.
           PERFORM     413-CASE-BEG        THRU    413-CASE-END.
           GO  TO      410-CASE-END.
       410-CASE-INSTMENT.
           PERFORM     414-CASE-BEG        THRU    414-CASE-END.
           GO  TO      410-CASE-END.
       410-CASE-TR-OVER.
           PERFORM     415-CASE-BEG        THRU    415-CASE-END.
           GO  TO      410-CASE-END.
       410-CASE-MVOUT.
           PERFORM     416-CASE-BEG        THRU    416-CASE-END.
           GO  TO      410-CASE-END.
      *---------------中途攤還７--------------------------------------
890922 410-CASE-MID-RTRN.                                               
           PERFORM     412-CASE-BEG        THRU    412-CASE-END.        
           GO  TO      410-CASE-END.                                    
       410-CASE-ERR.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      410-CASE-END.
       410-CASE-END.
           EXIT.
------*---------------催收、採積數計息：檔案資料檢驗------------------
       410-0-AR-OVERDUE-PROC.
           IF         ( NOT  LNP-C-GENERAL )                            
           THEN                                                         
050627       IF       ( LNL-ACC-CODE  =  '47' OR '48' OR '49' )         
"            THEN                                                       
"                       NEXT    SENTENCE                                
"            ELSE                                                       
                        MOVE    'S609'           TO   MSG-P-OUT-CODE    
                        PERFORM 9146-LN-ERR-MSG-OUT-RTN.                
           PERFORM     410-0-CASE-BEG      THRU    410-0-CASE-END.
       410-0-EXIT.
           EXIT.
------*---------------催收、採積數計息：限收回(1)、攤還(2)----------
       410-0-CASE-BEG.
           IF        ( W-CTF-C-RTRN )                                   
                       PERFORM 411-0-CASE-BEG    THRU  411-0-CASE-END   
           ELSE IF   ( W-CTF-C-RECV )                                   
                       PERFORM 412-0-CASE-BEG    THRU  412-0-CASE-END   
           ELSE                                                         
                       MOVE    'M686'            TO   MSG-P-OUT-CODE    
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
       410-0-CASE-END.
           EXIT.
------*---------------催收、採積數計息＆收回(1)：計息---------------
       411-0-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  411-0-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  411-0-CASE-REVERSE.
           GO  TO    411-0-CASE-ERR.
       411-0-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT = LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           IF          CTF-INT-AMT  NOT =  0
           THEN
"            IF   ((CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R) 
"                   OR  (CTF-INT-END-DATE-N-R  <  CTF-INT-BGN-DATE-N-R))
             THEN
               MOVE    'M643'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
      *---------------積數計息                                        
           IF          (LNP-C-ACCUM)
           THEN
             PERFORM   4901-0-CAL-ACCUM-RTN    THRU  4901-EXIT-RTN.
      *---------------寫ＬＮＩ（催收無利息不寫）                      
           IF         (CTF-INT-AMT  NOT =  0)
                   OR (CTF-FINE     NOT =  0)
           THEN
             PERFORM   4901-0-AR-INT-NML   THRU    4901-0-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000113     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
      *---------------寫LNR、更新主檔欄位、處理LOG-AFT欄位        
           PERFORM     9143-LN-ESTB-LNR-RTN.                            
           PERFORM     4902-0-AR-UPDATE-NML THRU   4902-0-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      411-0-CASE-END.
       411-0-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
      *---------------寫ＬＮＩ（催收無利息不寫）                      
           IF          (CTF-INT-AMT  NOT =  0)
                   OR  (CTF-FINE     NOT =  0)
           THEN
             PERFORM   4908-0-AR-INT-RVS   THRU    4908-0-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           MOVE        0                   TO      W-DIFF.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      411-0-CASE-END.
       411-0-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      411-0-CASE-END.
       411-0-CASE-END.
           EXIT.
------*---------------催收、採積數計息＆〔２攤還〕：計息處理----------
       412-0-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  412-0-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  412-0-CASE-REVERSE.
           GO  TO    412-0-CASE-ERR.
       412-0-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT <  LNL-LN-BAL)
           THEN                                                         
                  MOVE   'M679'    TO      MSG-P-OUT-CODE               
                  PERFORM   9146-LN-ERR-MSG-OUT-RTN.                    
           IF          (CTF-INT-AMT  NOT =  0)
           THEN
"            IF    ((CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R)
"                   OR  (CTF-INT-END-DATE-N-R  <  CTF-INT-BGN-DATE-N-R))
             THEN
               MOVE    'M643'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
           IF          (LNP-C-ACCUM)
           THEN
             PERFORM   4901-0-CAL-ACCUM-RTN    THRU  4901-EXIT-RTN.
      *---------------處理ＬＮＩ                                      
           IF          (CTF-INT-AMT  NOT =  0)
                    OR (CTF-FINE     NOT =  0)
           THEN
               PERFORM 4901-0-AR-INT-NML   THRU    4901-0-EXIT.
      *---------------處理有效利率調整ＬＮＩ                          
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
      *---------------處理ＬＮＲ／ＬＮＬ／ＬＯＧ...等              6
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-0-AR-UPDATE-NML THRU   4902-0-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      412-0-CASE-END.
      *---------------沖正處理----------------------------------------
       412-0-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           IF          (CTF-INT-AMT  NOT =  0)
                    OR (CTF-FINE     NOT =  0)
           THEN
             PERFORM   4908-0-AR-INT-RVS   THRU    4908-0-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           MOVE        0                   TO      W-DIFF.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      412-0-CASE-END.
       412-0-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      412-0-CASE-END.
       412-0-CASE-END.
           EXIT.
------*---------------積數計息(繳完息者不計)------------------------
       4901-0-CAL-ACCUM-RTN.
961206     IF  NOT   (LNL-LAST-TXN-DATE   =    TWA-TXN-DAT7)            
           THEN
             PERFORM   9171-LN-INT-AREA-CLEAR
             MOVE      ZERO                TO      INT-P-ABNOR-AUM-DATE
             MOVE      ZERO                TO      INT-P-ABNOR-AUM-AMT
             PERFORM  4901-PREPARE-ACCUM-PROG THRU 4901-PREPARE-EXIT
             MOVE      INT-P-NEW-RATE-ADJ-DATE TO LNL-NEXT-INT-ADJ-DATE
             MOVE      INT-P-ADJ-AUM-N     TO      LNL-AUM
             MOVE      INT-P-ADJED-RATE    TO      LNL-INT-RATE
961206       MOVE      TWA-TXN-DAT7        TO      LNL-LAST-TXN-DATE.   
       4901-EXIT-RTN.
           EXIT.
------*---------------積數計息----------------------------------------
       4901-PREPARE-ACCUM-PROG.
           MOVE        INT-M-FUN-CODE-25   TO      INT-P-FUN-CODE.
           MOVE        SPACES              TO      INT-P-FUN-SUB-CODE.
           MOVE        INT-M-EXEC-I        TO      INT-F-SERV-TYPE-INTC.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R     
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.
           MOVE        LNP-INT-ADJ-INTVL   TO      INT-P-RATE-ADJ.
           MOVE        LNP-RECV-INT-TYPE   TO      INT-P-RECV-INT-TYPE.
911230     MOVE        LNL-ACC-CODE         TO   INT-P-ACC-CODE.        
980513*---------------設定最低利率                                    
980513     IF          LNL-LMIT-RATE  NOT NUMERIC                       
           THEN                                                         
                       MOVE    ZEROS       TO  LNL-LMIT-RATE.           
980513     MOVE        LNL-LMIT-RATE       TO INT-P-LMIT-RATE.          
A10423*---------------改３００段一開始即判斷是否採日息計算            
"          IF        ( TBL27-MARK  =  'Y' )                             
"          THEN                                                         
920107                 MOVE  ZEROS          TO   INT-P-ACC-CODE.        
           MOVE        LNP-LN-CODE         TO      INT-P-LN-CODE.       
           MOVE        LNP-CT-TYPE         TO      INT-P-CT-TYPE.
           MOVE        LNL-INT-CODE        TO      INT-P-RATE-CODE.
           MOVE        LNL-INT-RATE        TO      INT-P-RATE.
           MOVE        LNL-INT-INCR-DECR-RATE TO   INT-P-INCR-DECR-RATE.
           MOVE        LNL-LN-BAL          TO      INT-P-LOAN-BAL.
           MOVE        LNL-AUM             TO      INT-P-ADJ-AUM.
791114     MOVE        LNL-LAST-TXN-DATE   TO   INT-P-L-TXN-DATE-R.
           MOVE        LNL-NEXT-INT-ADJ-DATE TO INT-P-NX-RATE-ADJ-DATE.
           IF          LNL-INT-ADJ-DD      =       LNL-1ST-LN-DD
           THEN
             MOVE      LNL-1ST-LN-DD       TO      INT-P-ADJ-BASIC-DD
           ELSE
             MOVE      LNL-BOOK-DD         TO      INT-P-ADJ-BASIC-DD.
           IF          LNP-C-ADJ-EACH-6-21
                   OR  LNP-C-ADJ-EACH-12-21
                   OR  LNP-C-ADJ-6-21-12-21
           THEN
             MOVE      21                  TO      INT-P-ADJ-BASIC-DD.
910731     IF        ( LNM-TAX-ID  =  LOW-VALUE OR SPACES OR ZEROS )    
"          THEN                                                         
"                      MOVE  '9'            TO   INT-P-ENT-TYPE         
"          ELSE                                                         
910731                 MOVE  '1'            TO   INT-P-ENT-TYPE.        
910808*---------------就學貸款８７年度前改為同企業戶一般繳息方式處理  
910808     IF        ( LNP-C-ENT-HGHER-SCH-LN )                         
"                AND ( NOT LNL-C-AR-OVERDUE )                           
990125           AND  NOT ( LNP-C-SCH-GEN )                             
"                AND ( LNP-RTRN-CODE  =  '3' )                          
"          THEN                                                         
"                      MOVE  '0'       TO   INT-P-RTRN-TYPE             
910808                 MOVE  '1'       TO   INT-P-ENT-TYPE.             
040107*---------------資金用途／初貸日／存單開發日                    
"          MOVE        LNP-CAPTL-PURPOSE    TO   INT-P-PURPOSE.         
"          MOVE        LNL-1ST-LN-DATE      TO   INT-P-1ST-DATE.        
090415     MOVE        LNP-CT-ISSUE-DATE    TO   INT-P-CT-ISSUE-DATE.   
      *---------------利息計算                                        
980506     EXEC CICS   LINK PROGRAM('NACINT7')                          
"                      COMMAREA(INT-GROUP) LENGTH(INT-LENGTH)           
"                      END-EXEC.                                        
           PERFORM     9165-INT-SUB-ERR-HDL-RTN.
       4901-PREPARE-EXIT.
            EXIT.
------*---------------催收、採積數計息：寫LNI ------------------------
       4901-0-AR-INT-NML.
050122*---------------排除：呆帳                                      
"          IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  4901-0-EXIT.                             
      *---------------計算期間日數                                    
           PERFORM     9171-LN-INT-AREA-CLEAR.
A91114     MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE-DATE.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.
           MOVE        DAT-M-FUN-1         TO      DAT-P-FUN-CODE.
"          MOVE    CTF-INT-BGN-DATE-N-R TO    DAT-P-BEG-DATE-R.         
900706     MOVE    CTF-INT-END-DATE-N-R TO    DAT-P-END-DATE-R.         
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
      *---------------利息LNI                                         
           PERFORM     9181-LN-LNI-CLEAR-RTN.
961206     MOVE        TWA-TXN-DAT7        TO      LNI-RECV-INT-DATE-R. 
"          MOVE        CTF-INT-BGN-DATE-N-R  TO    LNI-INT-BGN-DATE-R.  
900706     MOVE        CTF-INT-END-DATE-N-R TO     LNI-INT-END-DATE-R.  
           MOVE        DAT-P-TERM-DD       TO      LNI-DAYS.
           MOVE        LNL-INT-RATE        TO      LNI-INT-RATE.
           MOVE        LNL-LN-BAL          TO      LNI-FOR-INT-AMT.
           MOVE        LNI-M-INT-NORMAL    TO      LNI-INT-CHAR.
           MOVE        LNI-M-NORMAL        TO      LNI-STATUS.
           MOVE        0                   TO      LNI-RECVABL-AMT.
           MOVE        CTF-INT-AMT         TO      LNI-RECVED-AMT.
--->QQ     COMPUTE     LNI-INT-FINE  =  CTF-INT-AMT  +  CTF-FINE.       
           MOVE        CTF-SUP-CODE        TO      LNI-SUP-CODE.
901109     MOVE        ZEROS                TO   LNI-REMARK.            
901109     MOVE        ZEROS                TO   LNI-LABOR-DELAY-INT.   
991203     MOVE        +0                  TO    LNI-RTRN-AMT.          
991203     MOVE        +0                  TO    LNI-NOT-YET-RTRN-FREE. 
991203     MOVE        +0                  TO    LNI-RTRN-FREE.         
           PERFORM     9096-LN-DBS-I7-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           ADD         1                   TO      LNL-LNI-NO.
           MOVE        LNI                 TO      WSI.
      *---------------違約金LNI                                       
           PERFORM     9181-LN-LNI-CLEAR-RTN.                           
           IF          (CTF-FINE  NOT =  0)
           THEN
961206       MOVE      TWA-TXN-DAT7        TO      LNI-RECV-INT-DATE-R  
"            MOVE      CTF-INT-BGN-DATE-N-R  TO    LNI-INT-BGN-DATE-R   
900706       MOVE      CTF-INT-END-DATE-N-R TO     LNI-INT-END-DATE-R   
             MOVE      DAT-P-TERM-DD       TO      LNI-DAYS
             MOVE      LNL-INT-RATE        TO      LNI-INT-RATE
             MOVE      LNL-LN-BAL          TO      LNI-FOR-INT-AMT
             MOVE      LNI-M-FINE-OVER     TO      LNI-INT-CHAR
             MOVE      LNI-M-NORMAL        TO      LNI-STATUS
             MOVE      CTF-FINE            TO      LNI-RECVED-AMT
             MOVE      CTF-SUP-CODE        TO      LNI-SUP-CODE
930616       MOVE        ZEROS                TO   LNI-REMARK           
930616       MOVE        ZEROS                TO   LNI-LABOR-DELAY-INT  
991203       MOVE        +0                TO    LNI-RTRN-AMT           
991203       MOVE        +0                TO    LNI-NOT-YET-RTRN-FREE  
991203       MOVE        +0                TO    LNI-RTRN-FREE          
             PERFORM   9096-LN-DBS-I7-RTN
             PERFORM   9105-LN-DBS-ERR-HDL-RTN
             ADD       1                   TO      LNL-LNI-NO
             MOVE      1                   TO      W-FINE-SW.
       4901-0-EXIT.
           EXIT.
------*---------------催收、採積數計息：更新主檔----------------------
       4902-0-AR-UPDATE-NML.
           IF        ( CTF-INT-AMT  NOT =  0 )                          
           THEN
900706       MOVE  CTF-INT-END-DATE-N-R  TO   LNL-L-RECV-INT-DATE-R.    
           PERFORM     49022-UPDATE-NML-RTRN       THRU    49022-EXIT.
           PERFORM     49023-0-AR-NML-FINAL-CHK    THRU    49023-0-EXIT.
           PERFORM     49024-WRITE-BACK            THRU    49024-EXIT.
       4902-0-EXIT.
           EXIT.
------*---------------更新主檔欄位------------------------------------
       49022-0-UPDATE-NML-RTRN.
961206     MOVE        TWA-TXN-DAT7        TO      LNL-LAST-RTRN-DATE-R.
"          MOVE        TWA-TXN-DAT7        TO      LNL-LAST-TXN-DATE.   
           MOVE        LNR-RTRN-BAL        TO      LNL-LN-BAL.
           MOVE        LNL-M-NORMAL        TO      LNL-STATUS.
      *---------------循環案件：加回未用額度                          
           IF        ( LNP-C-CYL-PAY )                                  
           THEN
130910*---------------排除：非本金轉催收或呆帳案件                    
"            IF      ( LNL-ACC-CODE  =  '47' OR '48' OR '49' OR '57'    
"                                    OR '58' )                          
"                AND ( LNL-BC-COLM   =  '1' OR '2' )                    
"            THEN                                                       
"                      NEXT    SENTENCE                                 
"            ELSE                                                       
                       ADD     W-CTF-AMT    TO   W-LNP-APRV-UNUSED-BAL  
           END-IF.                                                      
961206     MOVE        TWA-TXN-DAT7        TO      LNM-LAST-TXN-DATE.   
           MOVE        0                   TO      LNM-BAL-SW.
       49022-0-EXIT.
           EXIT.
------*---------------催收、採積數計息：主檔資料檢驗更新--------------
       49023-0-AR-NML-FINAL-CHK.
           IF          (LNL-LN-BAL  NOT =  0)
090122             OR  (W-CTF-C-SCH-RTRN)                               
           THEN                                                         
                       GO  TO              49023-0-EXIT.                
           IF          (LNL-RECV-RTRN-DIFF  NOT =  0)
           THEN
             MOVE      'W606'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           SUBTRACT    1                   FROM    LNP-LN-NO.
           IF          (W-CTF-C-RTRN)
           THEN
             MOVE      LNL-M-RTRN          TO      LNL-STATUS
           ELSE
             MOVE      'D603'              TO      MSG-P-OUT-CODE
             PERFORM    9146-LN-ERR-MSG-OUT-RTN.
       49023-0-EXIT.
           EXIT.
------*---------------催收、採積數計息：沖正更正違約金LNI ------------
       4908-0-AR-INT-RVS.
050122*---------------排除：呆帳                                      
"          IF        ( LNL-ACC-CODE  =  '48' OR '49' )                  
"          THEN                                                         
"                      GO  TO  4908-0-EXIT.                             
"     *    ENDIF                                                        
      *---------------讀原ＬＮＩ                                      
           PERFORM     9091-LN-DBS-I2-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
900706     IF       (((LNI-INT-END-DATE-R  NOT =  CTF-INT-END-DATE-N-R) 
791114              AND  (LNI-INT-END-DATE-R NOT = REV-RECV-INT-DATE))
                    OR  (NOT  LNI-C-NORMAL)
                    OR  (NOT  LNI-C-INT-NORMAL)
                    OR  (CTF-INT-AMT  NOT =  LNI-RECVED-AMT))
           THEN
             MOVE      0                   TO      W-SWITCH
             PERFORM   49081-GET-NML-INT   THRU    49081-EXIT
               UNTIL   (W-SWITCH  =  1).
791114     IF    (LNI-INT-END-DATE-R  >  LNL-L-RECV-INT-DATE-R)
           THEN
             MOVE      'S610'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN
           ELSE
791114       IF   ((LNI-INT-END-DATE-R < LNL-L-RECV-INT-DATE-R)
  "             AND  (LNI-INT-END-DATE-R  NOT =  REV-RECV-INT-DATE))
             THEN
               MOVE    'M617'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
      *---------------更新原ＬＮＩ：改為錯消                          
           PERFORM     49082-0-AR-RVS-INT-REC      THRU    49082-0-EXIT.
           PERFORM     49083-0-AR-FINE-OTHER-CHK   THRU    49083-0-EXIT.
       4908-0-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49082-0-AR-RVS-INT-REC.
           MOVE        LNI-M-ERR-DLT       TO      LNI-STATUS.
           PERFORM     9094-LN-DBS-I5-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
991203     IF     ( LNI-C-EIR-REC )                                     
991203     THEN                                                         
991203       GO    TO   49082-0-EXIT.                                   
           ADD         1                   TO      W-LNI-NO.
9911       IF          (LNI-C-NORMAL)                                   
           THEN
             MOVE      LNI-RECVABL-AMT     TO      AFT-RECVABL-INT-AMT
           ELSE
             MOVE      LNI-RECVABL-AMT     TO      AFT-RECVABL-FINE-AMT.
       49082-0-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49083-0-AR-FINE-OTHER-CHK.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490831-GET-FINE-OTHER-INT   THRU    490831-EXIT
             UNTIL     (W-SWITCH  NOT = 0).
           IF          (W-SWITCH = 2)      GO  TO  49083-0-EXIT.
           IF          ((W-SWITCH = 1)
991203             AND  (NOT LNI-C-FINE-OVER)                           
991203             AND  (NOT LNI-C-EIR-REC))                            
           THEN
             MOVE      'Z601'              TO      MSG-P-OUT-CODE       
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49082-0-AR-RVS-INT-REC      THRU    49082-0-EXIT.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490832-GET-OTHER-INT        THRU    490832-EXIT
             UNTIL     (W-SWITCH = 2).
       49083-0-EXIT.
           EXIT.
------*---------------免息戶處理--------------------------------------
       410-1-INT-FREE-PROC.
           PERFORM     410-1-CASE-BEG      THRU    410-1-CASE-END.
       410-1-EXIT.
           EXIT.
       410-1-CASE-BEG.
           IF        ( W-CTF-C-RTRN    OR W-CTF-C-TR                    
                  OR   W-CTF-C-TR-OVER OR W-CTF-C-MVOUT )               
                       PERFORM 411-1-CASE-BEG    THRU  411-1-CASE-END   
           ELSE IF   ( W-CTF-C-RECV )                                   
121017                 PERFORM 412-1-CASE-BEG    THRU  412-1-CASE-END   
           ELSE                                                         
                       MOVE    'M686'            TO   MSG-P-OUT-CODE    
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
       410-1-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       411-1-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  411-1-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  411-1-CASE-REVERSE.
           GO  TO    411-1-CASE-ERR.
       411-1-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT = LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-INT-AMT  NOT =  0)
930616     AND  NOT    (LNP-LN-CODE  =  65 AND  LNL-ACC-CODE = 44 )     
           THEN
"            IF    ((CTF-INT-BGN-DATE-N-R NOT = LNL-L-RECV-INT-DATE-R)  
"                OR  (CTF-INT-END-DATE-N-R  <  CTF-INT-BGN-DATE-N-R))   
             THEN
               MOVE    'M643'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-INT-AMT  NOT =  0)
                  OR   (CTF-FINE     NOT =  0)
           THEN
               PERFORM 4901-0-AR-INT-NML   THRU    4901-0-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-1-INT-FREE-UPDATE-NML  THRU   4902-1-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      411-1-CASE-END.
       411-1-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           IF          (CTF-INT-AMT  NOT =  0)
                   OR  (CTF-FINE     NOT =  0)
           THEN
             PERFORM   4908-0-AR-INT-RVS   THRU    4908-0-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           MOVE        0                   TO      W-DIFF.
           IF          (W-CTF-C-MVOUT)
           THEN
             MOVE      REV-L-RECV-INT-DATE TO     LNL-L-RECV-INT-DATE
             COMPUTE   LNL-LNI-NO = LNL-LNI-NO - W-LNI-NO
             PERFORM   4161-MVOUT-UPDATE   THRU    4161-EXIT
           ELSE
             PERFORM   4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      411-1-CASE-END.
       411-1-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      411-1-CASE-END.
       411-1-CASE-END.
           EXIT.
       412-1-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  412-1-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  412-1-CASE-REVERSE.
           GO  TO    412-1-CASE-ERR.
       412-1-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT <  LNL-LN-BAL)
           THEN
960522         IF      (LNL-ACC-CODE      =  48)                        
               THEN                                                     
                        NEXT  SENTENCE                                  
               ELSE                                                     
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-INT-AMT  NOT =  0)
930616     AND  NOT    (LNP-LN-CODE  =  65 AND  LNL-ACC-CODE = 44 )     
           THEN
"            IF    ((CTF-INT-BGN-DATE-N-R NOT = LNL-L-RECV-INT-DATE-R)  
"                OR  (CTF-INT-END-DATE-N-R  <  CTF-INT-BGN-DATE-N-R))   
             THEN
               MOVE    'M643'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-INT-AMT  NOT =  0)
                    OR (CTF-FINE     NOT =  0)
           THEN
               PERFORM 4901-0-AR-INT-NML   THRU    4901-0-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-1-INT-FREE-UPDATE-NML  THRU   4902-1-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      412-1-CASE-END.
       412-1-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           IF          (CTF-INT-AMT  NOT =  0)
                   OR  (CTF-FINE     NOT =  0)
           THEN
             PERFORM   4908-0-AR-INT-RVS   THRU    4908-0-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           MOVE        0                   TO      W-DIFF.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      412-1-CASE-END.
       412-1-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      412-1-CASE-END.
       412-1-CASE-END.
           EXIT.
       4902-1-INT-FREE-UPDATE-NML.
           IF          ((W-CTF-C-RTRN)
                    OR  (W-CTF-C-RECV)
                    OR  (W-CTF-C-TR))      GO  TO  4902-1-CASE-OTHERS.
           IF          (W-CTF-C-TR-OVER)   GO  TO  4902-1-CASE-TR-OVER.
           IF          (W-CTF-C-MVOUT)     GO  TO  4902-1-CASE-MVOUT.
           GO  TO      4902-1-CASE-ERR.
       4902-1-CASE-OTHERS.
           IF        ( CTF-INT-AMT  NOT =  0 )                          
           THEN
900706       MOVE   CTF-INT-END-DATE-N-R  TO   LNL-L-RECV-INT-DATE-R.   
           PERFORM     49022-0-UPDATE-NML-RTRN     THRU    49022-0-EXIT.
           IF          (NOT W-CTF-C-RECV)
           THEN
             PERFORM   49023-NML-FINAL-CHK         THRU    49023-EXIT.
           PERFORM     49024-WRITE-BACK            THRU    49024-EXIT.
           GO  TO      4902-1-EXIT.
       4902-1-CASE-TR-OVER.
           MOVE        0                   TO      W-DIFF.
           PERFORM     4152-TR-OVER-UPDATE-NML     THRU    4152-EXIT.
           GO  TO      4902-1-EXIT.
       4902-1-CASE-MVOUT.
           IF        ( CTF-INT-AMT  NOT =  0 )                          
           THEN
900706       MOVE  CTF-INT-END-DATE-N-R  TO  LNL-L-RECV-INT-DATE-R.     
           PERFORM     4161-MVOUT-UPDATE   THRU    4161-EXIT.
           GO  TO      4902-1-EXIT.
       4902-1-CASE-ERR.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4902-1-EXIT.
       4902-1-EXIT.
           EXIT.
------*---------------〔１〕清償收回----------------------------------
       411-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  411-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  411-CASE-REVERSE.
           GO  TO      411-CASE-ERR.
       411-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT = LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     4111-CASE-BEG       THRU    4111-CASE-END.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      411-CASE-END.
       411-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           PERFORM     4112-CASE-BEG       THRU    4112-CASE-END.
           GO  TO      411-CASE-END.
       411-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      411-CASE-END.
       411-CASE-END.
           EXIT.
------*---------------收回(1)正常交易-------------------------------
       4111-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4111-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 4111-CASE-PRINCIPAL.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4111-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4111-CASE-PRINCIPAL. 
           GO  TO      4111-CASE-ERR.
       4111-CASE-GENERAL.                                               
           PERFORM     4901-INT-NML-G      THRU    4901-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-UPDATE-NML-G   THRU    4902-EXIT.
           GO  TO      4111-CASE-END.
       4111-CASE-PRINCIPAL.
           PERFORM     4903-INT-NML-P      THRU    4903-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4904-UPDATE-NML-P   THRU    4904-EXIT.
           GO  TO      4111-CASE-END.
       4111-CASE-ANNUITY.                                               
           IF           ((CTF-INT-AMT = 0)
961206             AND  (LNL-L-RECV-INT-DATE-R < TWA-TXN-DAT7)          
                   AND  (TWA-C-SUP-OFF)                                 
961226             AND  (LNL-ACC-CODE NOT = 48 OR 49 ))                 
           THEN
980720       MOVE     SPACES                TO   MSG-T282               
"            MOVE 'M640 利息金額錯誤'     TO   MSG-T282-LINE1         
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
           PERFORM     4905-INT-NML-A      THRU    4905-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4906-UPDATE-NML-A   THRU    4906-EXIT.
           GO  TO      4111-CASE-END.
       4111-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4111-CASE-END.
       4111-CASE-END.
           EXIT.
------*---------------收回(1)沖正交易-------------------------------
       4112-CASE-BEG.
           IF          (WKS-C-GENERAL
                    OR  WKS-C-FIX-PRINCIPAL) GO TO 4112-CASE-G-P.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4112-CASE-G-P.       
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4112-CASE-ANNUITY.
           GO  TO      4112-CASE-ERR.
       4112-CASE-G-P.
961206     IF       (( REV-L-RECV-INT-DATE < TWA-TXN-DAT7)              
                 AND ( REV-L-RECV-INT-DATE < LNL-L-RECV-INT-DATE))      
911101         OR                                                       
961206              (( REV-L-RECV-INT-DATE < TWA-TXN-DAT7)              
"                AND ( LNP-C-ENT-HGHER-SCH-LN )                         
990125           AND  NOT ( LNP-C-SCH-GEN )                             
"                AND ( LNP-RTRN-CODE            =  '3' )                
911101           AND ( NOT   LNL-C-AR-OVERDUE ))                        
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4112-CASE-END.
       4112-CASE-ANNUITY.
961206     IF      ((REV-L-RECV-INT-DATE < TWA-TXN-DAT7)                
                AND  (REV-L-RECV-INT-DATE < LNL-L-RECV-INT-DATE))
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4112-CASE-END.
       4112-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4112-CASE-END.
       4112-CASE-END.
           EXIT.
------*---------------攤還２、中途攤還７------------------------------
       412-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  412-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  412-CASE-REVERSE.
           GO  TO      412-CASE-ERR.
      *---------------正常交易                                        
       412-CASE-NORMAL.
040317*---------------先以原攤還方式：計算應攤還本金                  
"          MOVE        ZEROS                TO   WK7-RTRNABL-AMT.       
040324     IF        ( TWA-C-TXN-NOT-REVERSE )                          
"                AND ( W-CTF-C-MID-RTRN )                               
"                AND ( CTF-INT-END-DATE-N-R  NOT < LNL-RECV-INT-DATE-R )
"          THEN                                                         
"                      PERFORM 487-RTRN-AMT-BEF-RTN   THRU  487-EXIT.   
"     *    ENDIF                                                        
      *---------------檢核：ＬＮＬ貸放狀況／攤還金額                  
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF        ( CTF-RECV-AMT  NOT <  LNL-LN-BAL )                
040317           AND ( LNL-ACC-CODE  NOT =  '48' )                      
"                AND ( LNL-ACC-CODE  NOT =  '49' )                      
060824           AND ( LNL-ACC-CODE  NOT =  '58' )                      
"          THEN                                                         
"                      MOVE    'M679'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
"     *    ENDIF                                                        
           PERFORM     4121-CASE-BEG       THRU    4121-CASE-END.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      412-CASE-END.
      *---------------沖正交易                                        
       412-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           PERFORM     4122-CASE-BEG       THRU    4122-CASE-END.
           GO  TO      412-CASE-END.
       412-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      412-CASE-END.
       412-CASE-END.
           EXIT.
------*---------------攤還２、７：正常交易----------------------------
       4121-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4121-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 4121-CASE-PRINCIPAL.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4121-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4121-CASE-PRINCIPAL. 
           GO  TO      4121-CASE-ERR.
      *---------------０：一般                                        
       4121-CASE-GENERAL.
           PERFORM     4901-INT-NML-G      THRU    4901-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-UPDATE-NML-G   THRU    4902-EXIT.
           GO  TO      4121-CASE-END.
      *---------------１／３：平均法                                  
       4121-CASE-PRINCIPAL.
           IF          (LNL-EACH-RTRN-AMT  =  0)
           THEN
             MOVE      'M689'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
      *---------------非分期攤還額的倍數需主管同意                    
           DIVIDE      CTF-RECV-AMT        BY      LNL-EACH-RTRN-AMT
             GIVING    W-QUOTIENT          REMAINDER  W-REMAINDER.
           IF          ((W-REMAINDER  NOT =  0)
                   AND  (TWA-C-SUP-OFF))
           THEN
980720                 MOVE  SPACES         TO   MSG-T282               
040317       MOVE 'M679B收回之本金錯誤：採〔平均攤還本金〕案件，'     
"                                                TO   MSG-T282-LINE1    
"            MOVE '     「攤還金額」必須為「分期攤還額」的倍數，'     
"                                                TO   MSG-T282-LINE2    
"            MOVE '    　非倍數攤還需主管同意！'                      
"                                                TO   MSG-T282-LINE2    
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
130521     IF          ( WKS-RTRN-TYPE = 7     )                        
"              AND     ( CTF-RECV-AMT  = ZEROS )                        
"              AND     ( CTF-INT-END-DATE-N  >=  LNL-NEXT-RTRN-DATE-R ) 
"              AND     ( TWA-C-SUP-OFF )                                
"          THEN                                                         
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE '     收回之本金錯誤：採〔按季攤還本金〕案件，'     
"                                                TO   MSG-T282-LINE1    
"            MOVE '     本次須償還本金，但「收回金額」為零元，'       
"                                                TO   MSG-T282-LINE2    
"            MOVE '     請檢視是否正確，需主管同意！'                 
"                                                TO   MSG-T282-LINE3    
130521       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
           PERFORM     4903-INT-NML-P      THRU    4903-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4904-UPDATE-NML-P   THRU    4904-EXIT.
           GO  TO      4121-CASE-END.
      *---------------〔攤還方式：２／４〕年金法                      
       4121-CASE-ANNUITY.
           PERFORM     4905-INT-NML-A      THRU    4905-EXIT.
991203     IF     ( LNL-EIR     NOT =  LNL-INT-RATE )                   
000131     AND     ( LNL-EIR       NOT =  ZEROS   )                     
991203     THEN                                                         
991203       PERFORM     4901-5-CAL-EIR-RTN   THRU   4901-5-EXIT.       
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4906-UPDATE-NML-A   THRU    4906-EXIT.
           GO  TO      4121-CASE-END.
       4121-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4121-CASE-END.
       4121-CASE-END.
           EXIT.
------*---------------攤還２、７：沖正交易----------------------------
       4122-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4122-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL
                    OR  WKS-C-FIX-ANNUITY)   GO TO 4122-CASE-P-A.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4122-CASE-P-A.       
           GO  TO      4122-CASE-ERR.
       4122-CASE-GENERAL.
961206     IF      (REV-L-RECV-INT-DATE  <  TWA-TXN-DAT7)               
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT
           ELSE
             COMPUTE   W-FOR-INT-AMT  =  REV-LN-BAL  -  CTF-RECV-AMT
             MOVE      0                   TO      W-SWITCH
             PERFORM   41221-GET-OTHER-INT-REC     THRU  41221-EXIT
               UNTIL   (W-SWITCH = 2).
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4122-CASE-END.
       4122-CASE-P-A.
           PERFORM     4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4122-CASE-END.
       4122-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4122-CASE-END.
       4122-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       41221-GET-OTHER-INT-REC.
050122*---------------排除：呆帳                                      
"          IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      MOVE    2                 TO   W-SWITCH          
"                      GO  TO  41221-EXIT.                              
"     *    ENDIF                                                        
           IF        ( CTF-INT-AMT          =  ZEROS )                  
                 AND ( CTF-FINE             =  ZEROS )                  
961206           AND ( REV-L-RECV-INT-DATE  =  TWA-TXN-DAT7 )           
           THEN
             MOVE      2                   TO      W-SWITCH
             GO TO     41221-EXIT.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNI        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNI.         
           PERFORM     9106-LN-DBS-ERR-HDL-RTN-BOTH.
           IF          (TWA-C-DBS-NO-REC-FOUND)
           THEN
             MOVE      2                   TO      W-SWITCH
           ELSE
             IF        (LNI-C-NORMAL
                   AND  LNI-FOR-INT-AMT  NOT >  W-FOR-INT-AMT)
             THEN
               MOVE    'M617'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN
             ELSE
               NEXT SENTENCE.
       41221-EXIT.
           EXIT.
------*---------------展期(3)-----------------------------------------
       413-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  413-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  413-CASE-REVERSE.
           GO  TO      413-CASE-ERR.
       413-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT = LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     4131-CASE-BEG       THRU    4131-CASE-END.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      413-CASE-END.
       413-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           PERFORM     4132-CASE-BEG       THRU    4132-CASE-END.
           GO  TO      413-CASE-END.
       413-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      413-CASE-END.
       413-CASE-END.
           EXIT.
------*---------------展期(3)正常交易-------------------------------
       4131-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4131-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 4131-CASE-PRINCIPAL.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4131-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4131-CASE-PRINCIPAL. 
           GO  TO      4131-CASE-ERR.
       4131-CASE-GENERAL.
           PERFORM     4901-INT-NML-G      THRU    4901-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4902-UPDATE-NML-G   THRU    4902-EXIT.
           GO  TO      4131-CASE-END.
       4131-CASE-PRINCIPAL.
           PERFORM     4903-INT-NML-P      THRU    4903-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4904-UPDATE-NML-P   THRU    4904-EXIT.
           GO  TO      4131-CASE-END.
       4131-CASE-ANNUITY.
           IF          (LNL-RECV-INT-DATE  NOT =  LNL-NEXT-RTRN-DATE)
           THEN
             MOVE      LNL-RECV-INT-DATE   TO      LNL-NEXT-RTRN-DATE.
           IF           (CTF-INT-AMT = 0)                               
961206           AND  (LNL-L-RECV-INT-DATE-R  <  TWA-TXN-DAT7)          
           THEN
960531         IF     (LNL-ACC-CODE = 48 OR 49 )                        
               THEN                                                     
                       NEXT  SENTENCE                                   
               ELSE                                                     
             MOVE      'M640'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     4905-INT-NML-A      THRU    4905-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4906-UPDATE-NML-A   THRU    4906-EXIT.
           GO  TO      4131-CASE-END.
       4131-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4131-CASE-END.
       4131-CASE-END.
           EXIT.
------*---------------展期(3)沖正交易-------------------------------
       4132-CASE-BEG.
           IF          (WKS-C-GENERAL
                    OR  WKS-C-FIX-PRINCIPAL) GO TO 4132-CASE-G-P.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4132-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4132-CASE-G-P.       
           GO  TO      4132-CASE-ERR.
       4132-CASE-G-P.
961206     IF     (REV-L-RECV-INT-DATE < TWA-TXN-DAT7)                  
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4132-CASE-END.
       4132-CASE-ANNUITY.
961206     IF      (REV-L-RECV-INT-DATE < TWA-TXN-DAT7)                 
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4132-CASE-END.
       4132-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4132-CASE-END.
       4132-CASE-END.
           EXIT.
------*---------------中途攤還(4) ------------------------------------
       414-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  414-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  414-CASE-REVERSE.
           GO  TO      414-CASE-ERR.
       414-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (LNP-C-GENERAL)
           THEN
             MOVE      'D603'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     4141-CASE-BEG       THRU    4141-CASE-END.
           GO  TO      414-CASE-END.
       414-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           PERFORM     4142-CASE-BEG       THRU    4142-CASE-END.
           GO  TO      414-CASE-END.
       414-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      414-CASE-END.
       414-CASE-END.
           EXIT.
------*---------------中途攤還(4)正常交易---------------------------
       4141-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4141-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 4141-CASE-PRINCIPAL.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 4141-CASE-ANNUITY.
           GO  TO      4141-CASE-ERR.
      *---------------４中途攤還／只繳息------------------------------
       4141-CASE-GENERAL.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4141-CASE-END.
      *---------------４中途攤還／平均法------------------------------
       4141-CASE-PRINCIPAL.
           IF          (LNL-EACH-RTRN-AMT  =  0)
           THEN
             MOVE      'M689'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           DIVIDE      CTF-RECV-AMT        BY      LNL-EACH-RTRN-AMT
             GIVING    W-QUOTIENT          REMAINDER  W-REMAINDER.
      *---------------非〔攤還本金〕之倍數須主管同意                  
           IF          ((W-REMAINDER  NOT =  0)
                   AND  (TWA-C-SUP-OFF))
           THEN
980720                 MOVE  SPACES         TO   MSG-T282               
040317       MOVE 'M679B收回之本金錯誤：採〔平均攤還本金〕案件，'     
"                                                TO   MSG-T282-LINE1    
"            MOVE '     「攤還金額」必須為「分期攤還額」的倍數，'     
"                                                TO   MSG-T282-LINE2    
"            MOVE '    　非倍數攤還需主管同意！' TO MSG-T282-LINE3    
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
           IF          (CTF-RECV-AMT NOT <  LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
961206     IF      (TWA-TXN-DAT7  >  LNL-L-RECV-INT-DATE-R)             
           THEN
             MOVE      'M622'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49011-CHK-INT-FINE  THRU    49011-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4904-UPDATE-NML-P   THRU    4904-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      4141-CASE-END.
      *---------------４中途攤還／年金法------------------------------
       4141-CASE-ANNUITY.
961206     IF      (TWA-TXN-DAT7  >  LNL-L-RECV-INT-DATE-R)             
           THEN
             MOVE      'M622'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49011-CHK-INT-FINE  THRU    49011-EXIT.
           IF          (CTF-RECV-AMT  NOT <  LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4906-UPDATE-NML-A   THRU    4906-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      4141-CASE-END.
       4141-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4141-CASE-END.
       4141-CASE-END.
           EXIT.
------*---------------中途攤還(4)沖正交易---------------------------
       4142-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 4142-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL
                    OR  WKS-C-FIX-ANNUITY)   GO TO 4142-CASE-P-A.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 4142-CASE-P-A.       
           GO  TO      4142-CASE-ERR.
       4142-CASE-GENERAL.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4142-CASE-END.
       4142-CASE-P-A.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      4142-CASE-END.
       4142-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      4142-CASE-END.
       4142-CASE-END.
           EXIT.
------*---------------〔５〕轉催收------------------------------------
       415-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  415-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  415-CASE-REVERSE.
           GO  TO      415-CASE-ERR.
       415-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT =  LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
920514     MOVE        LNL-INT-RATE        TO      WS-BEF-47-INT.       
           PERFORM     4151-TR-OVER-INT-NML        THRU    4151-EXIT.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4152-TR-OVER-UPDATE-NML     THRU    4152-EXIT.
           PERFORM     4907-PREPARE-AFT            THRU    4907-EXIT.
           GO  TO      415-CASE-END.
       415-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           IF          (CTF-INT-AMT  NOT = 0)
           THEN
             PERFORM   4908-INT-RVS        THRU    4908-EXIT.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4910-UPDATE-RVS     THRU    4910-EXIT.
           GO  TO      415-CASE-END.
       415-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      415-CASE-END.
       415-CASE-END.
           EXIT.
------*---------------轉催收(5)正常交易-----------------------------
       4151-TR-OVER-INT-NML.
           IF          (CTF-INT-AMT = 0)
                  AND  (CTF-FINE    = 0)                                
                  AND ( WK-LABOR-DELAY-INT  =  ZEROS )                  
           THEN                                                         
                        GO  TO  4151-EXIT.                              
890522     IF          LNM-C-ACC-LN-SCHOOL                              
"                AND   LNP-C-ENT-HGHER-SCH-LN                           
"          THEN                                                         
"                 NEXT   SENTENCE                                       
"          ELSE                                                         
"          IF      (CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R  
"                OR  CTF-INT-END-DATE-N-R  <  CTF-INT-BGN-DATE-N-R)     
           THEN
             MOVE      'M643'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49012-CALCULATE-INT THRU    49012-EXIT.
           PERFORM     49013-ESTB-NML-LNI  THRU    49013-EXIT.
050122*---------------排除〔呆帳〕                                    
"          IF        ( LNL-ACC-CODE  =  '48' OR '49' )                  
"          THEN                                                         
"                      GO  TO  4151-EXIT.                               
"     *    ENDIF                                                        
      *---------------處理違約ＬＮＩ                                  
           IF          (WS-INT-FINE   = 0
                   AND  CTF-FINE = 0)      GO  TO  4151-EXIT.
           PERFORM     9145-LN-ESTB-FINE-LNI-RTN.
           MOVE        1                   TO      W-FINE-SW.
       4151-EXIT.
           EXIT.
------*---------------轉催收(5)沖正交易-----------------------------
       4152-TR-OVER-UPDATE-NML.
           IF          (CTF-INT-AMT  NOT =  0)
           THEN
             COMPUTE   LNL-RECV-RTRN-DIFF = LNL-RECV-RTRN-DIFF + W-DIFF.
           PERFORM     490231-CASE-BEG     THRU    490231-CASE-END.
961206     MOVE        TWA-TXN-DAT7        TO      LNM-LAST-TXN-DATE.   
           MOVE        0                   TO      LNM-BAL-SW.
           SUBTRACT    1                   FROM    LNP-LN-NO.
           PERFORM     49024-WRITE-BACK    THRU    49024-EXIT.
       4152-EXIT.
           EXIT.
------*---------------轉籍移出(6) ------------------------------------
       416-CASE-BEG.
           IF          (TWA-C-TXN-NOT-REVERSE)  GO TO  416-CASE-NORMAL.
           IF          (TWA-C-TXN-REVERSE)      GO TO  416-CASE-REVERSE.
           GO  TO      416-CASE-ERR.
      *---------------轉籍移出／正常交易------------------------------
       416-CASE-NORMAL.
           PERFORM     9129-LNL-STATUS-CHK-RTN-N-W.
           IF          (CTF-RECV-AMT  NOT =  LNL-LN-BAL)
           THEN
             MOVE      'M679'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49011-CHK-INT-FINE  THRU    49011-EXIT.
           IF          ((LNL-RECV-RTRN-DIFF  NOT =  0)
                   AND  (TWA-C-SUP-OFF))
           THEN
980720                 MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M60T 應補收退金額未消除'    TO   MSG-T282-LINE1    
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
           ELSE
             MOVE      '1'                 TO      AFT-R9.
           PERFORM     9143-LN-ESTB-LNR-RTN.
           PERFORM     4161-MVOUT-UPDATE   THRU    4161-EXIT.
           PERFORM     4907-PREPARE-AFT    THRU    4907-EXIT.
           GO  TO      416-CASE-END.
      *---------------轉籍移出／沖正交易------------------------------
       416-CASE-REVERSE.
           PERFORM     9131-LNL-STATUS-CHK-RTN-WE-EE.
           PERFORM     4909-RTRN-RVS       THRU    4909-EXIT.
           PERFORM     4161-MVOUT-UPDATE   THRU    4161-EXIT.
           GO  TO      416-CASE-END.
       416-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      416-CASE-END.
       416-CASE-END.
           EXIT.
------*---------------〔６〕轉籍移出：更新主檔欄位--------------------
       4161-MVOUT-UPDATE.
           PERFORM     41611-CASE-BEG      THRU    41611-CASE-END.
       4161-EXIT.
           EXIT.
       41611-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  41611-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  41611-CASE-REVERSE.
           GO  TO    41611-CASE-ERR.
      *---------------〔６〕轉籍移出：更新主檔欄位（正常交易）--------
       41611-CASE-NORMAL.
           PERFORM     490231-CASE-BEG     THRU    490231-CASE-END.
961206     MOVE        TWA-TXN-DAT7        TO      LNM-LAST-TXN-DATE.   
           MOVE        0                   TO      LNM-BAL-SW.
           SUBTRACT    1                   FROM    LNP-LN-NO.
           IF          LNP-C-CYL-PAY
           THEN
             COMPUTE   W-LNP-APRV-UNUSED-BAL = W-LNP-APRV-UNUSED-BAL +
                                             W-CTF-AMT.
           PERFORM     49024-WRITE-BACK    THRU    49024-EXIT.
           GO  TO      41611-CASE-END.
      *---------------〔６〕轉籍移出：更新主檔欄位（沖正交易）--------
       41611-CASE-REVERSE.
           MOVE        LNL-STATUS          TO      W-LNL-STATUS.
           MOVE        REV-LNL-STATUS      TO      LNL-STATUS.
           COMPUTE     LNL-LNR-NO  =  LNL-LNR-NO  -  W-LNR-NO.
           MOVE        REV-RTRN-INTVL      TO      LNP-RTRN-INTVL.
           MOVE        REV-LAST-TXN-DATE   TO      LNM-LAST-TXN-DATE.
           MOVE        REV-BAL-SW          TO      LNM-BAL-SW.
           ADD         1                   TO      LNP-LN-NO.
           IF          (LNP-C-CYL-PAY)
           THEN
             COMPUTE  W-LNP-APRV-UNUSED-BAL = W-LNP-APRV-UNUSED-BAL -
                                             W-CTF-AMT.
060113*---------設定：〔ＬＮＭ帳戶餘額異動數〕                        
"          IF        ( LNL-LN-BAL  NOT <  ZEROS )                       
"                AND ( REV-LN-BAL  NOT <  ZEROS )                       
"                      MOVE    CTF-RECV-AMT      TO   WK-LNM-TXN-AMT    
"          ELSE IF   ( LNL-LN-BAL      <  ZEROS )                       
"                AND ( REV-LN-BAL  NOT >  ZEROS )                       
"                      MOVE    ZEROS             TO   WK-LNM-TXN-AMT    
"          ELSE IF   ( LNL-LN-BAL      <  ZEROS )                       
"                AND ( REV-LN-BAL      >  ZEROS )                       
"                      MOVE    REV-LN-BAL        TO   WK-LNM-TXN-AMT    
"     *---------------※以下狀況不可能發生                            
"          ELSE                                                         
"                      MOVE    ZEROS             TO   WK-LNM-TXN-AMT.   
"     *    ENDIF                                                        
      *---------------更新主檔                                        
           PERFORM     49024-WRITE-BACK    THRU    49024-EXIT.
           GO  TO      41611-CASE-END.
       41611-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      41611-CASE-END.
       41611-CASE-END.
           EXIT.
------*---------------中途攤還７計算：應攤還本金----------------------
940506 487-RTRN-AMT-BEF-RTN.                                            
"     *---------------排除：非攤還案件／預繳案件／催呆科目            
"          IF        ( WK7-WKS-RTRN-CODE  =  '0' )                      
"                 OR ( LNL-EACH-RTRN-AMT  =  ZEROS )                    
961206            OR ( LNL-L-RECV-INT-DATE-R  NOT <  TWA-TXN-DAT7)      
040317            OR ( LNL-ACC-CODE  =  '47' OR '57' OR '48' OR '49'    
060824                               OR '58' )                          
940506     THEN                                                         
"                      GO  TO  487-EXIT.                                
"     *    ENDIF                                                        
940506*---------------排除：學貸分號                                  
"          IF        ( LNM-C-ACC-LN-SCHOOL )                            
"                AND ( LNP-C-ENT-HGHER-SCH-LN )                         
990125           AND ( NOT  LNP-C-SCH-GEN )                             
"          THEN                                                         
"                      GO  TO  487-EXIT.                                
040317*---------------計算：本次攤還期數                              
"          MOVE        ZEROS                TO   WKS-RTRN-TERM-NO.      
"          PERFORM     360-RTRN-TERM-CAL-RTN     THRU  360-EXIT.        
940506*---------------計息模組：起始設定                              
"          PERFORM     9171-LN-INT-AREA-CLEAR.                          
"          MOVE        'I'                  TO   INT-F-SERV-TYPE-INTC.  
"          MOVE        TWA-TXN-ID-CODE      TO  INT-F-TXN-ID-CODE.      
961206     MOVE        TWA-TXN-DAT7         TO  INT-F-TXN-DATE-R.       
"          MOVE        TWA-BR-CODE          TO  INT-F-BR-CODE.          
040317*---------------設定：攤還期間                                  
"          IF        ( LNP-RTRN-INTVL  =  '01' )                        
"                      MOVE    01                TO   INT-P-INTERVAL-MM 
"          ELSE IF   ( LNP-RTRN-INTVL  =  '02' )                        
"                      MOVE    06                TO   INT-P-INTERVAL-MM 
"          ELSE                                                         
"                      MOVE    01                TO   INT-P-INTERVAL-MM.
"     *    ENDIF                                                        
040317*---------------平均法                                          
"          IF        ( WK7-WKS-RTRN-CODE  =  '1' OR '3' )               
"                      COMPUTE WK7-RTRNABL-AMT   =    LNL-EACH-RTRN-AMT 
"                                                  *  WKS-RTRN-TERM-NO  
040317*---------------年金法                                          
"          ELSE IF   ( WK7-WKS-RTRN-CODE  =  '2' OR '4' )               
"                      MOVE    INT-M-FUN-CODE-24 TO   INT-P-FUN-CODE    
"                      MOVE    LNL-EACH-RTRN-AMT TO                     
"                                                INT-P-PER-RETURN-AMT   
"                      MOVE    LNP-RTRN-INTVL    TO   INT-P-INTERVAL-MM 
"                      MOVE    'Y'               TO   WK7-BF-MARK       
"                      PERFORM 490121-INT-PROC-COMM   THRU  490121-EXIT 
"                      MOVE    SPACES            TO   WK7-BF-MARK       
"          ELSE                                                         
"                      NEXT    SENTENCE.                                
"     *    ENDIF                                                        
"      487-EXIT.                                                        
"          EXIT.                                                        
------*---------------一般攤還：檢驗金額起訖日、計息------------------
       4901-INT-NML-G.
           PERFORM     49010-CASE-BEG      THRU    49010-CASE-END.
       4901-EXIT.
           EXIT.
       49010-CASE-BEG.
961206     IF          (LNL-L-RECV-INT-DATE-R  >  TWA-TXN-DAT7)         
                       GO  TO              49010-CASE-GT.
961206     IF          (LNL-L-RECV-INT-DATE-R  =  TWA-TXN-DAT7)         
                       GO  TO              49010-CASE-EQ.
           GO  TO      49010-CASE-LT.
      *---------------已預先繳息：不計息                              
       49010-CASE-GT.                                                   
050614     IF        ( TWA-BR-CODE  NOT =  '026' )                      
"          THEN                                                         
                       MOVE    'W612'            TO   MSG-P-OUT-CODE    
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
      *    ENDIF                                                        
           PERFORM     49011-CHK-INT-FINE  THRU    49011-EXIT.
           GO  TO      49010-CASE-END.
      *---------------已繳息：不計息                                  
       49010-CASE-EQ.
           PERFORM     49011-CHK-INT-FINE  THRU    49011-EXIT.
           GO  TO      49010-CASE-END.
      *---------------正常計息                                        
       49010-CASE-LT.
"          IF      (CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R) 
           THEN
060824         IF    ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
               THEN                                                     
                    NEXT  SENTENCE                                      
               ELSE                                                     
             MOVE      'M643'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
961206     IF           (CTF-INT-END-DATE-N-R  NOT =  TWA-TXN-DAT7)     
                   AND  (NOT W-CTF-C-TR-OVER)                           
           THEN
060824            IF    ( LNL-ACC-CODE  =  48 OR 49 OR '58' )           
                  THEN                                                  
                         NEXT SENTENCE                                  
                  ELSE                                                  
               MOVE    'M644'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
830201     IF      NOT   LNP-C-ENT-HGHER-SCH-LN
890209              OR   ((LNM-C-ACC-LN-NORMAL)                         
"                  AND   (LNP-C-ENT-HGHER-SCH-LN))                      
           THEN
961206       IF        ((TWA-TXN-DAT7    NOT >  LNL-RECV-INT-DATE-R)    
  "                AND  (TWA-TXN-DAT7    NOT >  WKS-DUE-DATE)           
                   AND  (CTF-FINE  NOT =  0))
             THEN
               MOVE    'M60I'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
      *---------------計息、建檔ＬＮＩ                                
           PERFORM     49012-CALCULATE-INT THRU    49012-EXIT.
           PERFORM     49013-ESTB-NML-LNI  THRU    49013-EXIT.
050122*---------------排除〔呆帳〕                                    
060824     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  49010-CASE-END.                          
"     *    ENDIF                                                        
      *---------------處理違約ＬＮＩ                                  
           IF          ((WS-INT-FINE   NOT =  0)
                   OR   (CTF-FINE NOT =  0))
           THEN
               PERFORM 9145-LN-ESTB-FINE-LNI-RTN
               MOVE    1                   TO      W-FINE-SW.
           GO  TO      49010-CASE-END.
       49010-CASE-END.
           EXIT.
------*---------------檢驗利息、違約金：必須為０----------------------
       49011-CHK-INT-FINE.
060824     IF        ( LNL-ACC-CODE  =  48 OR 49 OR '58' )              
"                AND ( WK-LABOR-DELAY-INT  NOT =  ZEROS )               
"          THEN                                                         
"                      MOVE   ZEROS         TO    WK-LABOR-DELAY-INT    
           END-IF.                                                      
901109     IF        ( WK-LABOR-DELAY-INT  NOT =  ZEROS )               
901109           AND ( WK-LABOR-DELAY-INT  NOT =  CTF-INT-AMT )         
901109           AND ( TWA-C-SUP-OFF )                                  
           THEN                                                         
060824           IF  ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
                 THEN                                                   
                       NEXT  SENTENCE                                   
                 ELSE                                                   
980720                 MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M640 利息金額錯誤'     TO   MSG-T282-LINE1         
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT.       
      *    ENDIF                                                        
           IF          (CTF-INT-AMT  NOT =  0)
901109           AND ( WK-LABOR-DELAY-INT  =  ZEROS )                   
           THEN
             MOVE      'M660'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           IF          (CTF-FINE  NOT =  0)
           THEN
             MOVE      'M60I'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
       49011-EXIT.
           EXIT.
------*---------------計息（共用的段落）------------------------------
       49012-CALCULATE-INT.
           PERFORM     49014-END-DATE-CHECK       THRU   49014-END-EXIT.
940913     PERFORM     490122-CHECK-DUE-DATE-RTN  THRU   490122-EXIT.   
      *---------------計息模組：起始設定                              
           PERFORM     9171-LN-INT-AREA-CLEAR.
           MOVE        'I'                 TO INT-F-SERV-TYPE-INTC.
           MOVE        TWA-TXN-ID-CODE     TO INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO INT-F-TXN-DATE-R.         
           MOVE        TWA-BR-CODE         TO INT-F-BR-CODE.
      *---------------計息                                            
           PERFORM     49012-CASE-BEG      THRU    49012-CASE-END.
      *---------------計算差額：應收利息與實收利息（１０元內不計）    
           COMPUTE     W-DIFF-2-DOLLAR = WS-INT - CTF-INT-AMT.
77701      IF          (W-DIFF-2-DOLLAR  < 11)
77701            AND   (W-DIFF-2-DOLLAR  > -11)
           THEN
             MOVE      CTF-INT-AMT         TO      WS-INT.
           COMPUTE     W-DIFF-2-DOLLAR = WS-INT-FINE - CTF-FINE.
77701      IF          (W-DIFF-2-DOLLAR  < 11)
77701            AND   (W-DIFF-2-DOLLAR  > -11)
           THEN
             MOVE      CTF-FINE            TO      WS-INT-FINE.
010509*-----------------------------------------------------------------
"     *〔聯貸案〕因主辦行之計息因素本行無法同步設定，                 
"     *　　　　　不檢核輸入利息與電腦計算之誤差                       
121017*〔催收呆帳〕不檢核應補收退差額                                 
010509*-----------------------------------------------------------------
"          IF        ( LNP-LN-CODE     =  '13' )                        
"                OR  ( LNP-UNION-MARK  =  '1' OR '2' )                  
121017           OR  ( LNL-ACC-CODE  =  '47' OR '57' )                  
"                OR  ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
010509     THEN                                                         
"                      MOVE    ZEROS             TO   W-DIFF-2-DOLLAR   
"                      MOVE    CTF-INT-AMT       TO   WS-INT            
"                      MOVE    CTF-FINE          TO   WS-INT-FINE       
           END-IF.                                                      
           IF          (WS-INT  NOT =  CTF-INT-AMT)
           THEN
             IF        (TWA-C-SUP-OFF)
960531           AND   (LNL-ACC-CODE NOT = 48)                          
             THEN
980720                 MOVE  SPACES         TO   MSG-T282               
"                      MOVE 'M640 利息金額錯誤' TO MSG-T282-LINE1     
040107                 PERFORM 9351-SUP-RESTART-RTN    THRU  9351-EXIT  
             ELSE
               MOVE    '1'                 TO      AFT-R1
               COMPUTE W-DIFF = W-DIFF + WS-INT - CTF-INT-AMT.
           IF          (WS-INT-FINE  NOT =  CTF-FINE)
           THEN
             IF         (TWA-C-SUP-OFF)
960920             AND  (LNL-ACC-CODE NOT = 48)                         
             THEN
980720                 MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M60I 違約金額與電腦算出不符' TO   MSG-T282-LINE1   
040107       PERFORM     9351-SUP-RESTART-RTN    THRU  9351-EXIT        
             ELSE
               MOVE    '1'                 TO      AFT-R2
               COMPUTE W-DIFF = W-DIFF + WS-INT-FINE - CTF-FINE.
      *    ENDIF                                                        
940506******************************************************************
"     *中途攤還７，檢核：輸入之攤還本金＜應攤還本金的狀況，           
"     *以避免分行為了不造成逾期放款，以此方式少收本金，處置如下：     
"     *　※需主管同意、※差額計入應補收退金額                         
"     ******************************************************************
"          IF        ( W-CTF-C-MID-RTRN )                               
"                AND ( CTF-RECV-AMT  <  WK7-RTRNABL-AMT )               
"          THEN                                                         
"            IF      ( TWA-C-SUP-OFF )                                  
"            THEN                                                       
980720                 MOVE  SPACES         TO   MSG-T282               
"              MOVE 'M62R中途攤還７之收回本金＜應攤還本金，需主管同意'
"                                           TO   MSG-T282-LINE1         
040107                 PERFORM 9351-SUP-RESTART-RTN   THRU  9351-EXIT   
"            ELSE                                                       
"                      COMPUTE W-DIFF  =    W-DIFF + WK7-RTRNABL-AMT    
"                                        -  CTF-RECV-AMT.               
040317*-----------------------------------------------------------------
"     *攤還２＆平均法攤還案件＆攤還金額＜應攤還金額：處理同前         
"     *-----------------------------------------------------------------
"          IF        ( W-CTF-RECV-TYPE    =  '2' )                      
"                AND ( WK7-WKS-RTRN-CODE  =  '1' OR '3' )               
"                AND ( CTF-RECV-AMT       <  WK7-RTRNABL-AMT )          
"          THEN                                                         
"            IF      ( TWA-C-SUP-OFF )                                  
"            THEN                                                       
"                      MOVE    SPACES         TO   MSG-T282             
"            MOVE '採平均攤還案件（查６０４交易之〔攤還方式〕確認），'
"                                                TO   MSG-T282-LINE1    
"            MOVE '鍵入之「收回金額」＜應攤還本金(ZZZ,ZZZ,ZZ9)，'   
"                                                TO   MSG-T282-LINE2    
"                      MOVE    WK7-RTRNABL-AMT   TO   T282-L2-AMT0      
"            MOVE '需主管同意才可放行，請查明確認！'                  
"                                                TO   MSG-T282-LINE3    
"                      PERFORM 9351-SUP-RESTART-RTN   THRU  9351-EXIT   
"            ELSE                                                       
"                      COMPUTE W-DIFF  =    W-DIFF +  WK7-RTRNABL-AMT   
"                                                  -  CTF-RECV-AMT.     
"     *    ENDIF                                                        
      *---------------逾期註記次數                                    
           IF            SW-C-FINE-NO
                   AND   CTF-FINE  NOT =  0
           THEN
             MOVE      0                  TO       SW-FINE-CHARGE
             MOVE      1                  TO       BEF-OVERDUE-SW.
           IF          SW-C-FINE-YES
           THEN
             ADD       1                   TO      LNL-OVERDUE-RECV-NO.
           IF          INT-P-FUN-CODE   NOT  =  24  GO  TO  49012-EXIT.
           IF     NOT  W-CTF-C-RECV   GO  TO  49012-EXIT.
      *---------------差額：應收本金與實收本金（２元內不計）          
           COMPUTE     W-DIFF-2-DOLLAR =   INT-P-PRNCPAL-THIS-INTVL
                                         - CTF-RECV-AMT.
           IF          (W-DIFF-2-DOLLAR  > 3)
                 OR    (W-DIFF-2-DOLLAR  < -3)
021121          AND  (  NOT LNP-C-SCH-GEN )                             
021121          AND  (  NOT LNP-C-ENT-HGHER-SCH-LN )                    
           THEN
               MOVE    'M679'              TO      MSG-P-OUT-CODE
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
       49012-EXIT.
             EXIT.                                                      
------*---------------計息：依攤還方式--------------------------------
       49012-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 49012-CASE-GENERAL.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 49012-CASE-PRINCIPAL.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 49012-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 49012-CASE-PRINCIPAL.
           GO  TO      49012-CASE-ERR.
      *---------------一般繳息０--------------------------------------
       49012-CASE-GENERAL.
           IF          (LNP-C-INT-FREE)
           THEN
             PERFORM   49011-CHK-INT-FINE  THRU    49011-EXIT
             GO  TO    49012-CASE-END.
           IF          (LNP-C-CT-R)
           THEN
             MOVE      INT-M-FUN-CODE-22   TO      INT-P-FUN-CODE
             MOVE      LNL-CT-INT-CODE     TO      INT-P-CT-RATE-CODE
             MOVE   LNL-CT-INCR-DECR-RATE  TO   INT-P-CT-INCR-DECR-RATE
           ELSE
             MOVE      INT-M-FUN-CODE-21   TO      INT-P-FUN-CODE.
           MOVE        WKS-RTRN-TYPE       TO      INT-P-RTRN-TYPE.
           PERFORM     490121-INT-PROC-COMM       THRU   490121-EXIT.
           GO  TO      49012-CASE-END.
      *---------------攤還本金１、３----------------------------------
       49012-CASE-PRINCIPAL.
110509     IF        ( WKS-RTRN-TYPE  =  '7' )                          
"          THEN                                                         
"            MOVE      '27'                TO      INT-P-FUN-CODE       
"            MOVE      LNL-NEXT-RTRN-DATE  TO      INT-P-RTRN-DATE      
"            MOVE      LNP-RTRN-INTVL      TO      INT-P-INTERVAL-MM    
"          ELSE                                                         
"            MOVE      INT-M-FUN-CODE-21   TO      INT-P-FUN-CODE.      
"     *    ENDIF                                                        
110509*    MOVE        INT-M-FUN-CODE-21   TO      INT-P-FUN-CODE.      
           MOVE        WKS-RTRN-TYPE       TO      INT-P-RTRN-TYPE.
           MOVE        LNL-EACH-RTRN-AMT   TO      INT-P-PER-RETURN-AMT.
           PERFORM     490121-INT-PROC-COMM       THRU   490121-EXIT.
           GO  TO      49012-CASE-END.
      *---------------攤還本息２、４----------------------------------
       49012-CASE-ANNUITY.
             PERFORM   49012-CASE-A1       THRU 49012-A1-EXIT.
           GO  TO      49012-CASE-END.
       49012-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49012-CASE-END.
       49012-CASE-END.
           EXIT.
------*---------------分期攤還設定攤還期間----------------------------
       49012-CASE-A1.
           MOVE        INT-M-FUN-CODE-24   TO      INT-P-FUN-CODE.
           MOVE        LNL-EACH-RTRN-AMT   TO      INT-P-PER-RETURN-AMT.
           MOVE        LNP-RTRN-INTVL      TO      INT-P-INTERVAL-MM.
           IF          LNP-C-RTRN-MONTH-6
           THEN
             MOVE      06                  TO      INT-P-INTERVAL-MM.
           PERFORM     490121-INT-PROC-COMM      THRU   490121-EXIT.
       49012-A1-EXIT.
           EXIT.
------*---------------介面欄位處理＆連結計息模組（共用段落）----------
       490121-INT-PROC-COMM.
      *---------------就學貸款（非催收＆非總號）：特別處理            
890209     IF          LNM-C-ACC-LN-SCHOOL                              
"                AND   LNP-C-ENT-HGHER-SCH-LN                           
990125           AND  NOT   LNP-C-SCH-GEN                               
           THEN
             PERFORM   49015-SCH-INT-PROC  THRU   49015-EXIT
             GO   TO   490121-EXIT.
      *---------------設定介面欄位                                    
           MOVE        SPACE               TO INT-P-FUN-SUB-CODE.
           MOVE        LNL-INT-CODE        TO INT-P-RATE-CODE.
           MOVE        LNP-INT-ADJ-INTVL   TO INT-P-RATE-ADJ.
           MOVE        LNP-RECV-INT-TYPE   TO INT-P-RECV-INT-TYPE.
911230     MOVE        LNL-ACC-CODE         TO   INT-P-ACC-CODE.        
980513*---------------設定最低利率                                    
980513     IF          LNL-LMIT-RATE  NOT NUMERIC                       
           THEN                                                         
                       MOVE    ZEROS       TO  LNL-LMIT-RATE.           
980513     MOVE        LNL-LMIT-RATE       TO INT-P-LMIT-RATE.          
A10423*---------------改３００段一開始即判斷是否採日息計算            
"          IF        ( TBL27-MARK  =  'Y' )                             
"          THEN                                                         
920107                 MOVE  ZEROS          TO   INT-P-ACC-CODE.        
930311*---------收回：平均攤還本息或助學總號：一律按日息計算          
"          IF        ( CTF-INQ-TYPE  =  '1' )                           
"          THEN                                                         
"            IF     (( LNM-C-ACC-LN-SCHOOL )                            
"                AND ( LNP-C-ENT-HGHER-SCH-LN )                         
990125           AND ( LNP-C-SCH-GEN ) )                                
"              OR                                                       
"                   (( LNP-RTRN-CODE  =  '2' OR '4' )                   
"                 OR ( LNP-RTRN-CODE  =  '9'                            
"                  AND LNP-RTRN-INTVL =  '01' ))                        
"            THEN                                                       
"                      MOVE  ZEROS          TO   INT-P-ACC-CODE.        
"     *---------------中途攤還一律採短期日息計算                      
"          IF        ( CTF-INQ-TYPE      =  '7' )                       
940506           AND ( WK7-BF-MARK   NOT =  'Y' )                       
930311     THEN                                                         
"                      MOVE  ZEROS          TO   INT-P-ACC-CODE.        
040324*--------公教國宅拍賣後採按日計息－只還本金；收回時收息         
040324     IF        ( LNM-C-ACC-LN-NATION       )                      
                AND  ( LNP-CT-INTVL    =    '01' )                      
  "        THEN                                                         
"                      MOVE  ZEROS          TO   INT-P-ACC-CODE.        
      *    ENDIF                                                        
           MOVE        LNP-LN-CODE         TO INT-P-LN-CODE.            
           MOVE        LNP-CT-TYPE         TO INT-P-CT-TYPE.
           MOVE        LNP-FINE-TYPE       TO INT-P-FINE-TYPE.
           MOVE        LNL-INT-RATE        TO INT-P-RATE.
           MOVE        LNL-INT-INCR-DECR-RATE TO INT-P-INCR-DECR-RATE.
           MOVE        LNL-LN-BAL          TO INT-P-LOAN-BAL.
           MOVE        LNL-RECV-INT-DATE   TO INT-P-INT-RECV-DATE.
           MOVE        LNL-NEXT-INT-ADJ-DATE TO INT-P-NX-RATE-ADJ-DATE.
"          MOVE        CTF-INT-BGN-DATE-N-R  TO INT-P-INT-BEG-DATE-R.   
900706     MOVE        CTF-INT-END-DATE-N-R  TO INT-P-INT-END-DATE-R.   
           MOVE        WKS-DUE-DATE        TO INT-P-DUE-DATE.
           IF          (LNL-INT-ADJ-DD  =  LNL-1ST-LN-DD)
           THEN
             MOVE      LNL-1ST-LN-DD       TO      INT-P-ADJ-BASIC-DD   
           ELSE
             MOVE      LNL-BOOK-DD         TO      INT-P-ADJ-BASIC-DD.  
           IF          LNP-C-ADJ-EACH-6-21
                   OR  LNP-C-ADJ-EACH-12-21
                   OR  LNP-C-ADJ-6-21-12-21
           THEN
             MOVE      21                  TO      INT-P-ADJ-BASIC-DD.
           MOVE        LNL-RECV-INT-DAY    TO      INT-P-RECV-BASIC-DD. 
910731     IF        ( LNM-TAX-ID = LOW-VALUE OR SPACES OR ZEROS )      
"          THEN                                                         
"                      MOVE  '9'            TO   INT-P-ENT-TYPE         
"          ELSE                                                         
910731                 MOVE  '1'            TO   INT-P-ENT-TYPE.        
910808*---------------就學貸款８７年度前改為同企業戶一般繳息方式處理  
910808     IF        ( LNP-C-ENT-HGHER-SCH-LN )                         
"                AND ( NOT LNL-C-AR-OVERDUE )                           
990125           AND  NOT ( LNP-C-SCH-GEN )                             
"                AND ( LNP-RTRN-CODE  =  '3' )                          
"          THEN                                                         
"                      MOVE  '0'       TO   INT-P-RTRN-TYPE             
910808                 MOVE  '1'       TO   INT-P-ENT-TYPE.             
      *    ENDIF                                                        
040324*--------公教國宅拍賣後採按日計息－只還本金；收回時收息         
040324     IF        ( LNM-C-ACC-LN-NATION       )                      
                AND  ( LNP-CT-INTVL    =    '01' )                      
  "        THEN                                                         
               IF      TWA-TXN-DAT7    >    INT-P-DUE-DATE              
               THEN                                                     
"                      MOVE  TWA-TXN-DAT7   TO   INT-P-INT-RECV-DATE    
"                      MOVE  TWA-TXN-DAT7   TO   INT-P-DUE-DATE.        
      *    ENDIF                                                        
040107*---------------資金用途／初貸日／存單開發日                    
"          MOVE        LNP-CAPTL-PURPOSE    TO   INT-P-PURPOSE.         
"          MOVE        LNL-1ST-LN-DATE      TO   INT-P-1ST-DATE.        
090415     MOVE        LNP-CT-ISSUE-DATE    TO   INT-P-CT-ISSUE-DATE.   
      *---------------連結：計息模組                                  
980506     EXEC        CICS             LINK    PROGRAM ('NACINT7')     
                       COMMAREA (INT-GROUP)     LENGTH (INT-LENGTH)
                       END-EXEC.
0606XX     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"            NEXT  SENTENCE                                             
"          ELSE                                                         
"            PERFORM     9165-INT-SUB-ERR-HDL-RTN.                      
A10423*---------------國宅固定攤還金額調整本金處理                    
A10423*---------------全部減逾期息、違約金、利息剩下為攤還本金        
           IF        ( F28-MARK   =   'Y'  )                            
           THEN                                                         
                       PERFORM  4901211-F28-PROC-RTN THRU 4901211-EXIT. 
940506*---------------中途攤還７第一次計算應攤還本金                  
"          IF        ( WK7-BF-MARK  =  'Y' )                            
"          THEN                                                         
"                      MOVE    INT-P-PRNCPAL-THIS-INTVL                 
"                                                TO   WK7-RTRNABL-AMT   
"                      GO  TO  490121-EXIT.                             
910823*---------------依加減碼分段計息特別處理                        
910823     IF        ( SWITCH-A  =  'Y' )                               
"          THEN                                                         
"                      MOVE  WK-INTP-THIS-RTRN-AMT                      
"                                      TO   INT-P-PRNCPAL-THIS-INTVL    
"                      MOVE  WK-INTP-INT         TO   INT-P-INT         
"                      MOVE  WK-INTP-INT-TOTAL   TO   INT-P-INT-TOTAL   
"                      MOVE  WK-INTP-INT-THIS-INTVL                     
"                                           TO   INT-P-INT-THIS-INTVL   
"                      MOVE  WK-INTP-INT-DELAY   TO   INT-P-INT-DELAY   
910823                 MOVE  WK-INTP-PENALTY     TO   INT-P-PENALTY.    
      *---------------到期日非營業＆次營業日＝交易日：不計違約金      
840913     IF          FINE-SW             =   'YES'                    
           THEN                                                         
             MOVE      +0                  TO      INT-P-PENALTY.       
880512     IF          (LNP-APRV-SER-NO  = 7777)                        
"                AND   (LNP-LN-CODE      = 78)                          
"          THEN                                                         
"            IF      ( LNL-ACC-CODE  =  '47' OR '57' )                  
"            THEN                                                       
"                      MOVE   ZEROS     TO    INT-P-PENALTY.            
981125     IF        ( F24-MARK = 'Y' )                                 
           THEN                                                         
                       MOVE   W-AMT-10  TO    INT-P-PRNCPAL-THIS-INTVL  
                       MOVE   W-AMT-8   TO    INT-P-INT-THIS-INTVL      
                                              INT-P-INT-TOTAL           
                       MOVE   ZEROS     TO    INT-P-INT-DELAY           
                       MOVE   ZEROS     TO    INT-P-PENALTY.            
                                                                        
841002     IF          (LNP-C-LONG-TERM-LN                              
             AND       LNP-C-HANDICAP-CRATE-G-LN)                       
             OR        LNP-C-SINGLE-HOME-LN                             
           THEN                                                         
110509       IF      ( INT-P-FUN-CODE  =  24 OR 23 OR 27 )              
"            THEN                                                       
             COMPUTE   INT-P-INT-THIS-INTVL   ROUNDED  =                
880506                 INT-P-INT-THIS-INTVL   /  2                      
"            COMPUTE   INT-P-INT-TOTAL  = INT-P-INT-DELAY               
"                                       + INT-P-INT-THIS-INTVL          
980114       ELSE                                                       
"                      COMPUTE INT-P-INT ROUNDED =    INT-P-INT / 2     
"                      COMPUTE INT-P-INT-TOTAL   =    INT-P-INT         
"                                                  +  INT-P-INT-DELAY.  
      *---------------違約金、逾期息                                  
           IF          SW-C-FINE-NO
           THEN
             MOVE      0                   TO     WS-INT-FINE.
           IF          SW-C-FINE-YES
           THEN
110509       IF        INT-P-FUN-CODE      =      24  OR  27            
             THEN
             COMPUTE   WS-INT-FINE         =      INT-P-PENALTY +
                                                  INT-P-INT-DELAY
           ELSE
             IF      ( INT-P-FUN-CODE  =  '21' OR '22' )                
             THEN
               MOVE    INT-P-PENALTY       TO     WS-INT-FINE.
901109*---------------補入失業勞工展延利息                            
901109     IF        ( WK-LABOR-DELAY-INT  >  ZEROS )                   
901109     THEN                                                         
901109                 ADD   WK-LABOR-DELAY-INT  TO   INT-P-INT-TOTAL   
901109         IF    ( INT-P-FUN-CODE  =  24 OR 23 )                    
901109         THEN                                                     
901109                 ADD   WK-LABOR-DELAY-INT TO INT-P-INT-THIS-INTVL 
901109         ELSE                                                     
901109                 ADD   WK-LABOR-DELAY-INT  TO   INT-P-INT.        
981125*---------------補入公教特別協議利息                            
           IF        ( F24-MARK   =   'C'  )                            
           THEN                                                         
                       ADD   W-AMT-2    TO   INT-P-INT-TOTAL            
               IF    ( INT-P-FUN-CODE  =  24 OR 23 )                    
               THEN                                                     
                       ADD   W-AMT-2        TO   INT-P-INT-THIS-INTVL   
               ELSE                                                     
                       ADD   W-AMT-2        TO   INT-P-INT.             
      *---------------正常息                                          
110509     IF          INT-P-FUN-CODE       =  24 OR 27                 
"          THEN                                                         
"            MOVE      INT-P-INT-THIS-INTVL  TO   WS-INT                
"          ELSE                                                         
110509       MOVE      INT-P-INT-TOTAL       TO   WS-INT.               
           MOVE        INT-P-INT-INTERVAL-DD TO   WS-NOR-DLY-DAYS.
           IF          INT-P-INT-DELAY     NOT  =  0
           THEN
             MOVE      1                     TO   END-DATE-SW
           ELSE
             MOVE      0                     TO   END-DATE-SW.
           IF          (CTF-ACC-BUSINS-CODE  = 69
                  AND   LNL-RECV-CODE  =  1)
           THEN
             MOVE      CTF-FINE             TO      WS-INT-FINE
             IF        CTF-FINE       NOT   =       ZERO                
             THEN
               MOVE      0                  TO      SW-FINE-CHARGE
               MOVE      1                  TO      BEF-OVERDUE-SW
             ELSE
               MOVE      1                  TO      SW-FINE-CHARGE
               MOVE      0                  TO      BEF-OVERDUE-SW.
910731     IF        ( W-CTF-C-MID-RTRN )                               
961206           AND ( LNL-L-RECV-INT-DATE-R  <  TWA-TXN-DAT7 )         
910731           AND ( INT-P-ADJED-RATE       NUMERIC )                 
           THEN                                                         
           MOVE        INT-P-ADJED-RATE TO      LNL-INT-RATE.           
021121     IF        ( LNP-C-ENT-HGHER-SCH-LN )                         
"                AND ( LNP-C-SCH-GEN)                                   
"                AND ( INT-P-FUN-CODE  =  24 OR 23 )                    
"                AND ( LNL-ACC-CODE    =  37 )                          
"                AND ( CTF-RECV-TYPE   =  2  )                          
"          THEN                                                         
"                      MOVE  CTF-RECV-AMT   TO INT-P-PRNCPAL-THIS-INTVL.
       490121-EXIT.
           EXIT.
A10423 4901211-F28-PROC-RTN.                                            
           EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN (4901211-EXIT)                           
"                      NOTFND  (4901211-EXIT)                           
"                      ERROR   (4901211-EXIT)                           
"                      INVREQ  (4901211-EXIT)                           
"                      ENDFILE (4901211-EXIT)                           
"                      END-EXEC.                                        
           MOVE        CTF-ACC-NO           TO   LNF28-ACC-NO.          
           MOVE        CTF-LN-ID-NO         TO   LNF28-LN-ID-NO.        
           EXEC  CICS  READ  DATASET('LNCTNRF')                         
                             INTO(LNF28-REC)                            
                             LENGTH(F28-LENGTH)                         
                             RIDFLD(LNF28-KEY)                          
           END-EXEC.                                                    
           IF          LNF28-FILE-STATUS    =    9                      
           THEN                                                         
                       GO    TO   4901211-EXIT.                         
           MOVE        +0         TO        LNF28-PRNCPAL.
           MOVE        +0         TO        LNF28-INT-AMT.
           MOVE        +0         TO        LNF28-DELAY.
           MOVE        +0         TO        LNF28-PENALTY.
           IF          WKS-TERM-NO  NOT     =    ZEROS
           THEN
                       COMPUTE  LNF28-PRNCPAL    =  WKS-TERM-NO
                                                 *  LNF28-EACH-AMT
           ELSE
                       MOVE   LNF28-EACH-AMT TO  LNF28-PRNCPAL.
           IF          INT-P-FUN-CODE  =  24 OR 23
           THEN
                       MOVE   INT-P-PENALTY TO   LNF28-PENALTY
                       MOVE   INT-P-INT-DELAY    TO LNF28-DELAY
                       MOVE   INT-P-INT-THIS-INTVL TO LNF28-INT-AMT     
           ELSE
                       MOVE   INT-P-PENALTY TO   LNF28-PENALTY
                       MOVE   INT-P-INT-TOTAL TO LNF28-INT-AMT.         
           COMPUTE     LNF28-PRNCPAL        =    LNF28-PRNCPAL
                                            -    LNF28-INT-AMT
                                            -    LNF28-DELAY
                                            -    LNF28-PENALTY.
           IF          LNF28-PRNCPAL NOT    >    ZEROS
           THEN
"                      MOVE  SPACES         TO   MSG-T282               
"            MOVE 'M663A交易６６２１每期攤還金額不足繳逾期息 '        
"                                           TO   MSG-T282-LINE1         
"            MOVE  '請查明後重新操作'          TO   MSG-T282-LINE2    
                       PERFORM 9146A-CHINESE-MSG-RTN  THRU 9146A-EXIT.  
           MOVE        LNF28-PRNCPAL TO   INT-P-PRNCPAL-THIS-INTVL.
       4901211-EXIT.
           EXIT.
------*--------------計算：到期日之營業日-----------------------------
840913 490122-CHECK-DUE-DATE-RTN.                                       
961206     IF          (LNL-DUE-YY         =       TWA-TXN-YY7)         
"             AND      (LNL-DUE-MM         =       TWA-TXN-MM7)         
           THEN                                                         
             NEXT      SENTENCE                                         
           ELSE                                                         
             GO TO     490122-EXIT.                                     
           PERFORM     9171-LN-INT-AREA-CLEAR.                          
           MOVE        10                  TO      DAT-P-FUN-CODE.      
           MOVE        SPACES              TO      INT-P-FUN-SUB-CODE.  
           MOVE        'L'                 TO    INT-P-FUN-SUB-CODE-L.  
           MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE.     
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.   
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.       
           MOVE        LNL-DUE-DATE-R      TO      DAT-P-SPECIAL-DATE-R.
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
           IF          DAT-P-DAYS          =       0001                 
           THEN                                                         
             GO  TO    490122-EXIT.                                     
           IF        ( LNL-L-RECV-INT-DATE-R  NOT =  LNL-DUE-DATE-R )   
           THEN                                                         
             GO  TO    490122-EXIT.                                     
           PERFORM     9171-LN-INT-AREA-CLEAR.                          
           MOVE        07                  TO      DAT-P-FUN-CODE.      
           MOVE        SPACES              TO      INT-P-FUN-SUB-CODE.  
           MOVE        'L'                 TO    INT-P-FUN-SUB-CODE-L.  
           MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE.     
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.   
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.       
           MOVE        LNL-DUE-DATE-R      TO      DAT-P-SPECIAL-DATE-R.
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
961206     IF          DAT-P-NEXT-BUSS-DATE-R =    TWA-TXN-DAT7         
           THEN                                                         
             MOVE      'YES'               TO      FINE-SW.             
       490122-EXIT.                                                     
           EXIT.                                                        
------*---------------尋找到期日前一日之營業日期----------------------
"      490122A-CHECK-DUE-DATE-RTN.                                      
"          PERFORM     9171-LN-INT-AREA-CLEAR.                          
"          MOVE        DAT-M-FUN-19        TO      DAT-P-FUN-CODE.      
"          MOVE        SPACES              TO      INT-P-FUN-SUB-CODE.  
"          MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE.     
"          MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.   
"          MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
"          MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.       
"          MOVE        TWA-TXN-DAT7        TO      DAT-P-BEG-DATE-R.    
"          MOVE        1                   TO      DAT-P-BUSS-DAYS.     
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
041229*-----------------------------------------------------------------
"     *設定〔不收違約金註記〕，條件如下：                             
"     *    •逢〔到期日＝假日〕                                       
"     *    •且〔交易日＝到期日之第１個營業日〕                       
"     *    •且〔應繳日〕＝〔到期日〕                                 
"     *-----------------------------------------------------------------
           IF        ( LNL-DUE-DATE-R         >  DAT-P-CORR-DATE )      
                 AND ( LNL-DUE-DATE-R         <  TWA-TXN-DAT7 )         
041229           AND ( LNL-RECV-INT-DATE  NOT <  LNL-DUE-DATE )         
           THEN                                                         
                       MOVE    1                 TO   SW-FINE-CHARGE    
                       MOVE    0                 TO   BEF-OVERDUE-SW.   
       490122A-EXIT.                                                    
           EXIT.                                                        
------*---------------處理ＬＮＩ--------------------------------------
       49013-ESTB-NML-LNI.
050122*---------------排除〔呆帳〕                                    
0606XX     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  49013-EXIT.                              
"     *    ENDIF                                                        
010514*---------------排除：〔無利息違約金〕且〔無起迄日〕            
"          IF        ( WS-INT       =  0 )                              
"                AND ( CTF-INT-AMT  =  0 )                              
"                AND ( CTF-FINE     =  0 )                              
"                AND ( CTF-INT-BGN-DATE-N-R  =  ZEROS )                 
"                AND ( CTF-INT-END-DATE-N-R  =  ZEROS )                 
"          THEN                                                         
"                      GO  TO  49013-EXIT.                              
"     *    ENDIF                                                        
040107*---------------設定：ＬＮＩ欄位（原為LNK144取消改為指令）    
           MOVE        ZEROS                TO   LNI.                   
970114     MOVE        TWA-TXN-DAT7         TO   LNI-RECV-INT-DATE-R.   
970626     MOVE        CTF-INT-BGN-DATE-N-R TO   LNI-INT-BGN-DATE-R.    
900706     MOVE        CTF-INT-END-DATE-N-R TO   LNI-INT-END-DATE-R.    
           MOVE        WS-NOR-DLY-DAYS      TO   LNI-DAYS.              
           MOVE        INT-P-RATE           TO   LNI-INT-RATE.          
040107*-----------八一氣爆房屋修繕補貼專案－資金用途５４：調整利率項目
"          IF        ( LNP-CAPTL-PURPOSE     =  '54' )                  
"                AND ( LNL-ACC-CODE      NOT =  '47' )                  
"                AND ( LNL-ACC-CODE      NOT =  '48' )                  
"          THEN                                                         
"                      MOVE    WK54-NEW-RATE     TO   LNI-INT-RATE.     
"     *    ENDIF                                                        
           MOVE        LNL-LN-BAL           TO   LNI-FOR-INT-AMT.       
           MOVE        LNI-M-INT-NORMAL     TO   LNI-INT-CHAR.          
           MOVE        LNI-M-NORMAL         TO   LNI-STATUS.            
           MOVE        WS-INT               TO   LNI-RECVABL-AMT.       
           MOVE        CTF-INT-AMT          TO   LNI-RECVED-AMT.        
           COMPUTE     LNI-INT-FINE =  CTF-INT-AMT  +  CTF-FINE.        
           IF        ( SW-C-FINE-YES )                                  
           THEN                                                         
                       MOVE  1         TO   LNI-OVERDUE-RECV-STATUS.    
      *    ENDIF                                                        
           MOVE        CTF-SUP-CODE         TO   LNI-SUP-CODE.          
901109     MOVE        ZEROS                TO   LNI-REMARK.            
901109     MOVE        ZEROS                TO   LNI-LABOR-DELAY-INT.   
901109     IF        ( WK-LABOR-DELAY-INT  NOT =  ZEROS )               
           THEN                                                         
                       MOVE  WK-LABOR-DELAY-INT TO LNI-LABOR-DELAY-INT. 
991213     MOVE        +0                  TO    LNI-RTRN-AMT.          
991213     MOVE        +0                  TO    LNI-NOT-YET-RTRN-FREE. 
991213     MOVE        +0                  TO    LNI-RTRN-FREE.         
      *---------------ＬＮＩ寫檔                                      
           PERFORM     9096-LN-DBS-I7-RTN.                              
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.                         
           ADD         1                    TO   LNL-LNI-NO.            
      *---------------清ＬＮＩ                                        
           MOVE        LNI                 TO      WSI.
           PERFORM     9181-LN-LNI-CLEAR-RTN.                           
       49013-EXIT.
           EXIT.
------*---------------計算：違約截止日，設定違約註記------------------
       49014-END-DATE-CHECK.
           IF          CTF-INT-BGN-DATE-N-R  >     CTF-INT-END-DATE-N-R 
           THEN                                                         
             MOVE      'M644'              TO      MSG-P-OUT-CODE       
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
      *---------------推算〔交易日＋７個營業日〕                      
           PERFORM     9171-LN-INT-AREA-CLEAR.
           MOVE        DAT-M-FUN-19        TO      DAT-P-FUN-CODE.
           MOVE        SPACES              TO      INT-P-FUN-SUB-CODE.
A91114     MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      DAT-P-BEG-DATE-R.    
           MOVE        7                   TO      DAT-P-BUSS-DAYS.
           IF          (CTF-ACC-BUSINS-CODE  =  69)
                  AND  (LNP-C-CTZN-HOUSE-LN
                    OR  LNP-C-LABOR-HOUSE-LN
                    OR  LNP-C-CTZN-MATCH-LN
77910               OR   LNP-C-GVR-MATCH-LN
                    OR  LNP-C-GVR-HOUSE-LN)
           THEN
             MOVE      LNP-FINE-TYPE       TO     WS-FINE-TYPE
             PERFORM   9102-SEAH-FINE-DAYS-RTN
             GO  TO    49014-END-EXIT.
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
      *---------------設定逾期註記                                    
           IF          (DAT-P-CORR-DATE    NOT >   LNL-RECV-INT-DATE)
961206            AND  (TWA-TXN-DAT7       NOT >   WKS-DUE-DATE)        
           THEN
             MOVE      1                   TO      SW-FINE-CHARGE
             MOVE      0                   TO      BEF-OVERDUE-SW
           ELSE
             MOVE      0                   TO      SW-FINE-CHARGE
             MOVE      1                   TO      BEF-OVERDUE-SW.
980817*---------------尋找到期日前一日之營業日期                      
"          IF          LNL-DUE-DATE-R       <    TWA-TXN-DAT7           
"          THEN                                                         
"            PERFORM   490122A-CHECK-DUE-DATE-RTN  THRU  490122A-EXIT.  
       49014-END-EXIT.
           EXIT.
------*---------------計息：就學貸款----------------------------------
       49015-SCH-INT-PROC.
           PERFORM     49015A-SCH-L-R-RTN          THRU     49015A-EXIT.
           PERFORM     49015B-SCH-DUE-RTN          THRU     49015B-EXIT.
      ******************************************************************
      *狀況一：無應繳利息                                             
      *狀況二：前半期已繳，計算半繳後半期                             
      *狀況三：前半期未繳，計息迄日＜到期日，計算半繳前半期           
      *狀況四：前半期未繳，計息迄日＞＝到期日，計算全繳二期           
      ******************************************************************
961206     IF          (LNL-L-RECV-INT-DATE-R      >   TWA-TXN-DAT7)    
           THEN
             PERFORM   490151-SCH-TERM-0-RTN       THRU     490151-EXIT
             GO   TO   49015-EXIT.
           IF          (LNL-RECV-INT-DATE-R       =   LNL-DUE-DATE-R)
                OR     (LNL-EACH-RTRN-AMT         =   LNL-LN-BAL)
           THEN
             PERFORM   490153-SCH-TERM-2-RTN       THRU     490153-EXIT
             GO   TO   49015-EXIT.
900706     IF          (CTF-INT-END-DATE-N-R      <   LNL-DUE-DATE-R)   
           THEN
             PERFORM   490152-SCH-TERM-1-RTN       THRU     490152-EXIT
             GO   TO   49015-EXIT.
           IF          (LNL-RECV-INT-DATE-R  NOT =   LNL-DUE-DATE-R)
              OR      ((LNL-EACH-RTRN-AMT    NOT >   LNL-LN-BAL)
900706        AND      (CTF-INT-END-DATE-N-R NOT <   LNL-DUE-DATE-R))   
           THEN
             PERFORM   490154-SCH-ALL-TERM-RTN     THRU      490154-EXIT
             GO   TO   49015-EXIT.
       49015-EXIT.
           EXIT.
------*---------------就學貸款：無利息--------------------------------
       490151-SCH-TERM-0-RTN.
           MOVE        +0                    TO   INT-P-INT-THIS-INTVL
                                                  INT-P-PENALTY
                                                  WS-INT
                                                  WS-INT-FINE.
831207     MOVE        WK-RECV-INT-DATE      TO  INT-P-NX-INT-RECV-DATE.
           MOVE        LNL-NEXT-INT-ADJ-DATE TO INT-P-NEW-RATE-ADJ-DATE.
           MOVE        LNL-INT-RATE          TO  INT-P-ADJED-RATE.
       490151-EXIT.
           EXIT.
------*---------------就學貸款：預定收息日＋６個月--------------------
960502*-----9605XX取消就學貸款預定收息日＋６個月--------------        
       49015A-SCH-L-R-RTN.
960502     MOVE        LNL-RECV-INT-DATE-R TO      WK-RECV-INT-DATE.    
       49015A-EXIT.
           EXIT.
------*---------------就學貸款：到期日＋６個月------------------------
960502*---9605XX　取消就學貸款：到期日＋６個月----------------------  
       49015B-SCH-DUE-RTN.
960502     MOVE        LNL-DUE-DATE-R      TO      WK-DUE-DATE.         
       49015B-EXIT.
           EXIT.
------*---------------就學貸款：半繳上半期（未到期）------------------
      ******************************************************************
      *收息方式-------------改為「按月收息」                        
      *現欠餘額-------------改為「分期攤還額」（現欠的一半）        
      *預定收息日-----------改為「原預定收息日＋６個月」            
960502*預定收息日-----------改為「原預定收息日」                    
      *計息迄日-------------改為「交易日」                          
      *---------------------------------------------------計息後      
      *下次收息日(INT-P)----改為「到期日」                          
      *公付-----------------不計「利息、逾期息」                    
      ******************************************************************
       490152-SCH-TERM-1-RTN.
         COPY LNK27152.                                                 
       490152-EXIT.
           EXIT.
       490153-SCH-TERM-2-RTN.
         COPY LNK27153.                                                 
       490153-EXIT.
           EXIT.
------*---------------就學貸款：全繳二期------------------------------
      ******************************************************************
      *半繳上半期(490152-RTN)＋半繳下半期(490153-RTN)               
      ******************************************************************
       490154-SCH-ALL-TERM-RTN.
         COPY LNK27154.                                                 
       490154-EXIT.
           EXIT.
------*---------------就學貸款：半繳上半期（未到期）------------------
911101******************************************************************
"     *收息方式-------------改為「按月收息」                        
"     *現欠餘額-------------改為「分期攤還額」（現欠的一半）        
"     *預定收息日-----------改為「原預定收息日＋６個月」            
960502*預定收息日-----------改為「原預定收息日」                    
"     *計息迄日-------------改為「交易日」                          
"     *攤還方式-------------改為「一般繳息」                        
"     *客戶型態-------------改為「企業戶」（ＦＯＲ違約金計算方式）  
"     *---------------------------------------------------計息後      
"     *下次收息日(INT-P)----改為「到期日」                          
"     *計息日數(INT-P)------改為二段累計後日數                      
"     *※以【911001】分界分二段計息                                 
"     ******************************************************************
911101 490161-SCH-DIFF-CAL1-RTN.                                        
         COPY LNK27161.                                                 
"      490161-EXIT.                                                     
911101        EXIT.                                                     
------*---------------就學貸款：半繳上半期（未到期）------------------
911101******************************************************************
"     *收息方式-------------改為「按月收息」                        
"     *預定收息日-----------改為「原預定收息日＋６個月」            
960502*預定收息日-----------改為「原預定收息日」                    
      *到期日---------------改為「到期日＋６個月」                  
960502*到期日---------------改為「到期日」                          
"     *計息迄日-------------改為「交易日」                          
"     *攤還方式-------------改為「一般繳息」                        
"     *客戶型態-------------改為「企業戶」（ＦＯＲ違約金計算方式）  
"     *---------------------------------------------------計息後      
      *下次收息日(INT-P)----改為「到期日＋６個月」                  
960502*下次收息日(INT-P)----改為「到期日」                          
"     *計息日數(INT-P)------改為二段累計後日數                      
"     *※以【911001】分界分二段計息                                 
"     ******************************************************************
911101 490162-SCH-DIFF-CAL2-RTN.                                        
         COPY LNK27162.                                                 
"      490162-EXIT.                                                     
911101        EXIT.                                                     
------*-----------------------------------------------------------------
       4902-UPDATE-NML-G.
961206     IF      ((LNL-L-RECV-INT-DATE-R  <  TWA-TXN-DAT7)            
970609          AND  (CTF-INT-BGN-DATE-N-R  NOT =  ZERO))               
900320     OR          (W-CTF-C-MID-RTRN)                               
           THEN
             PERFORM   49021-UPDATE-NML-INT        THRU    49021-EXIT.
           PERFORM     49022-UPDATE-NML-RTRN       THRU    49022-EXIT.
           PERFORM     49023-NML-FINAL-CHK         THRU    49023-EXIT.
           PERFORM     49024-WRITE-BACK            THRU    49024-EXIT.
       4902-EXIT.
           EXIT.
------*---------------更新：收息日、調整日、補收退金額、利率----------
       49021-UPDATE-NML-INT.
           IF     NOT  LNP-C-ENT-HGHER-SCH-LN
A112XX             OR  ((LNM-C-ACC-LN-SCH-INT )                         
"                 AND  (LNP-C-ENT-HGHER-SCH-LN))                        
900813             OR  ((LNP-C-ENT-HGHER-SCH-LN)                        
990125           AND   ( LNP-C-SCH-GEN ))                               
           THEN
060417       IF     ( CTF-INT-END-DATE-N-R  NOT  =  ZEROS )
                AND ( CTF-INT-END-DATE-N-R NOT < LNL-L-RECV-INT-DATE-R)
             THEN
060417          MOVE    CTF-INT-END-DATE-N-R TO  LNL-L-RECV-INT-DATE-R. 
910731     IF        ( W-CTF-C-MID-RTRN )                               
961206           AND ( WK-L-RECV-INT-DATE  NOT <  TWA-TXN-DAT7 )        
           THEN                                                         
             PERFORM    490211-CASE-BEG     THRU    490211-CASE-END     
             GO         TO          49021-EXIT.                         
           COMPUTE     LNL-RECV-RTRN-DIFF = LNL-RECV-RTRN-DIFF + W-DIFF.
           MOVE        INT-P-NEW-RATE-ADJ-DATE  TO
                                                LNL-NEXT-INT-ADJ-DATE.
980525     IF          INT-P-ADJED-RATE <       LNL-LMIT-RATE           
           THEN                                                         
                       MOVE  LNL-LMIT-RATE    TO  LNL-INT-RATE          
                       MOVE  LNL-LMIT-RATE    TO  INT-P-ADJED-RATE      
           ELSE                                                         
                       MOVE  INT-P-ADJED-RATE TO  LNL-INT-RATE.         
           PERFORM     490211-CASE-BEG     THRU    490211-CASE-END.
       49021-EXIT.
             EXIT.                                                      
------*---------------更新：預定收息日--------------------------------
       490211-CASE-BEG.
           IF          (WKS-C-GENERAL)       GO TO 490211-CASE-G.
           IF          (WKS-C-FIX-PRINCIPAL) GO TO 490211-CASE-P.
           IF          (WKS-C-FIX-ANNUITY)   GO TO 490211-CASE-ANNUITY.
110509     IF          ( WKS-RTRN-TYPE = 7 ) GO TO 490211-CASE-P.       
           GO  TO      490211-CASE-ERR.
       490211-CASE-G.
910731     IF        ( W-CTF-C-MID-RTRN )                               
961206           AND ( WK-L-RECV-INT-DATE  NOT <  TWA-TXN-DAT7 )        
           THEN                                                         
              GO   TO    490211-CASE-G-SPEC.                            
050614     IF        ( INT-P-NX-INT-RECV-DATE  >  ZEROS )               
           MOVE        INT-P-NX-INT-RECV-DATE   TO LNL-RECV-INT-DATE.   
           IF          INT-P-NX-INT-RECV-DATE    > WKS-DUE-DATE
           THEN
             IF        BEF-L-RECV-INT-DATE  <      WKS-DUE-DATE
             THEN
               MOVE    WKS-DUE-DATE        TO      LNL-RECV-INT-DATE
             ELSE
               MOVE    INT-P-NX-INT-RECV-DATE      TO LNL-RECV-INT-DATE.
920708*---------------預先一次收息、收手續費：以交易日當預定收息日    
"          IF       (( LNP-C-RECV-CMS )                                 
"                 OR ( LNP-C-INT-RECV-ADVANCE ))                        
"              AND   ( LNL-RECV-INT-DATE-R  =  ZEROS )                  
"          THEN                                                         
961206                 MOVE  TWA-TXN-DAT7   TO   LNL-RECV-INT-DATE-R.   
890922     IF          (W-CTF-C-MID-RTRN)                               
           THEN                                                         
              GO   TO    490211-CASE-G-SPEC.                            
           IF          (NOT   LNP-C-TERM-AVER-RTRN)
                  OR   (W-CTF-C-RTRN)
                  OR   (W-CTF-C-TR)
           THEN
             GO  TO    490211-CASE-END.
------*---------------中途攤還：設定〔利率／分期攤還額〕--------------
       490211-CASE-G-SPEC.                                              
981221     IF        ( W-CTF-C-MID-RTRN )                               
"                AND ( WK-L-RECV-INT-DATE  NOT <  TWA-TXN-DAT7 )        
"          THEN                                                         
"                      NEXT    SENTENCE                                 
"          ELSE                                                         
980525       IF        INT-P-ADJED-RATE <       LNL-LMIT-RATE           
             THEN                                                       
                       MOVE LNL-LMIT-RATE    TO      LNL-INT-RATE       
                       MOVE LNL-LMIT-RATE    TO      INT-P-ADJED-RATE   
             ELSE                                                       
                       MOVE INT-P-ADJED-RATE TO      LNL-INT-RATE.      
      *    ENDIF                                                        
      *---------------計算分期攤還額（中途攤還）                      
890922     IF            (BEF-RTRN-STYLE   =   2 )                      
  "        THEN                                                         
031013                 PERFORM 4902A-ECHR-CAL-RTN     THRU  4902A-EXIT  
"                      GO  TO  490211-CASE-END.                         
"     *    ENDIF                                                        
      *---------------中途攤還＆超前付息：不調整〔下繳日〕            
910731     IF        ( W-CTF-C-MID-RTRN )                               
961206           AND ( WK-L-RECV-INT-DATE  NOT <  TWA-TXN-DAT7 )        
           THEN                                                         
           GO  TO      490211-CASE-END.                                 
950321     IF   (LNL-L-RECV-INT-DATE-R  <  LNP-TERM-BGN-RTRN-DATE-R )   
              AND (INT-P-NX-INT-RECV-DATE  NOT >  WKS-DUE-DATE)
           THEN
950321       IF  INT-P-NX-INT-RECV-DATE-R NOT < LNP-TERM-BGN-RTRN-DATE-R
             THEN
950321         MOVE LNP-TERM-BGN-RTRN-DATE-R TO LNL-RECV-INT-DATE-R     
             ELSE
950321       IF  INT-P-NX-INT-RECV-DATE-R < LNP-TERM-BGN-RTRN-DATE-R    
050614           AND ( INT-P-NX-INT-RECV-DATE  >  ZEROS )               
             THEN
               MOVE  INT-P-NX-INT-RECV-DATE  TO   LNL-RECV-INT-DATE.
031013*    ENDIF                                                        
"     *---------------跨寬限期                                        
950321     IF   (LNL-L-RECV-INT-DATE-R NOT < LNP-TERM-BGN-RTRN-DATE-R ) 
               AND (INT-P-NX-INT-RECV-DATE  NOT >  WKS-DUE-DATE)
           THEN
             MOVE    LNP-M-RTRN-MONTH-1     TO      LNP-RTRN-INTVL
900706       MOVE    CTF-INT-END-DATE-N-R TO   LNL-RECV-INT-DATE-R      
             PERFORM 9167-LN-INT-DATE-ADJ-RTN                           
             MOVE    LNL-RECV-INT-DATE      TO      LNL-NEXT-RTRN-DATE
             MOVE    ZERO                   TO      LNL-EACH-RTRN-AMT
031013                 PERFORM 4902A-ECHR-CAL-RTN     THRU  4902A-EXIT. 
"     *    ENDIF                                                        
           GO  TO      490211-CASE-END.
------*---------------〔１〕攤還本金----------------------------------
       490211-CASE-P.
           IF           (LNR-RTRN-BAL  NOT =  0)
050614           AND ( INT-P-NX-INT-RECV-DATE  >  ZEROS )               
           THEN
             MOVE      INT-P-NX-INT-RECV-DATE   TO LNL-RECV-INT-DATE.
           GO  TO      490211-CASE-END.
031013*---------------〔２〕攤還本息----------------------------------
       490211-CASE-ANNUITY.
           IF          (LNR-RTRN-BAL  NOT = 0)
050614           AND ( INT-P-NX-INT-RECV-DATE  >  ZEROS )               
           THEN
             MOVE      INT-P-NX-INT-RECV-DATE   TO LNL-RECV-INT-DATE.
           GO  TO      490211-CASE-END.
       490211-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490211-CASE-END.
       490211-CASE-END.
           EXIT.
------*---------------重算〔分期攤還額〕------------------------------
031013 4902A-ECHR-CAL-RTN.                                              
"     *---------------設定：介面區參數                                
"          MOVE        ZEROS                TO   ECHR-AMT-CAL-AREA.     
"          MOVE        LNL-EACH-RTRN-AMT    TO   ECHR-EACH-RTRN-AMT.    
"          MOVE        LNR-RTRN-BAL         TO   ECHR-LN-BAL.           
"          MOVE        LNL-RECV-INT-DAY     TO   ECHR-RECV-DAY.         
"          MOVE        LNL-INT-RATE         TO   ECHR-INT-RATE.         
031013     MOVE        LNL-L-RECV-INT-DATE  TO   ECHR-L-RECV-DATE.      
110509     IF        ( LNP-RTRN-CODE  =  7 )                            
"          THEN                                                         
"                      MOVE LNL-NEXT-RTRN-DATE-R TO   ECHR-N-RECV-DATE  
"          ELSE                                                         
"                      MOVE  LNL-RECV-INT-DATE   TO   ECHR-N-RECV-DATE  
"          END-IF.                                                      
031013     MOVE        WKS-DUE-DATE         TO   ECHR-DUE-DATE.         
"          MOVE        LNP-RTRN-INTVL       TO   ECHR-TERM-UNIT.        
"          IF        ( LNP-RTRN-INTVL  =  '02' )                        
"          THEN                                                         
"                      MOVE    06                TO   ECHR-TERM-UNIT.   
"     *    ENDIF                                                        
031013*---------------平均攤還本金                                    
110509     IF        ( LNP-RTRN-CODE  =  '1' OR '3' OR '7' )            
"              OR                                                       
"                   (( LNP-RTRN-CODE             =  '9'  )              
"                AND ( LNP-RTRN-INTVL            >  ZEROS )             
"                AND ( LNP-AFTER-TERM-RTRN-TYPE  =  '1'  ))             
"                      MOVE    '1'               TO   ECHR-TYPE         
031013*---------------平均攤還本息                                    
"          ELSE IF   ( LNP-RTRN-CODE  =  '2' OR '4' )                   
"              OR   (( LNP-RTRN-CODE             =  '9'  )              
"                AND ( LNP-RTRN-INTVL            >  ZEROS )             
031211           AND ( LNP-AFTER-TERM-RTRN-TYPE  =  '2'  ))             
"                      MOVE    '2'               TO   ECHR-TYPE         
"          ELSE                                                         
"                      GO  TO  4902A-EXIT.                              
"     *    ENDIF                                                        
031013*---------------計算：分期攤還額                                
"          PERFORM     9267-ECHR-AMT-CAL-RTN     THRU  9267-EXIT.       
031013*---------------更新：分期攤還額                                
"          IF        ( ECHR-RETURN  =  ZEROS )                          
"          THEN                                                         
"                      MOVE  ECHR-EACH-RTRN-AMT  TO   LNL-EACH-RTRN-AMT.
"     *    ENDIF                                                        
"      4902A-EXIT.                                                      
"            EXIT.                                                      
------*---------------更新主檔欄位------------------------------------
       49022-UPDATE-NML-RTRN.
110509*---------------攤還本金為０＝＞不更新上次攤還日                
"          IF        ( CTF-RECV-AMT  >  ZEROS   )                       
"          THEN                                                         
"               MOVE   TWA-TXN-DAT7        TO      LNL-LAST-RTRN-DATE-R.
110509*    ENDIF                                                        
"          MOVE        TWA-TXN-DAT7        TO      LNL-LAST-TXN-DATE.   
           MOVE        LNR-RTRN-BAL        TO      LNL-LN-BAL.
830111     IF          LNP-C-ENT-HGHER-SCH-LN
890209         AND     LNM-C-ACC-LN-SCHOOL                              
               AND     (CTF-RECV-TYPE      =       2)
           THEN
821227       MOVE      LNR-RTRN-BAL        TO      LNL-EACH-RTRN-AMT.
           MOVE        LNL-M-NORMAL        TO      LNL-STATUS.
      *---------------循環案件：加回未用額度                          
           IF          LNP-C-CYL-PAY
           THEN                                                         
130910*---------------排除：非本金轉催收或呆帳案件                    
"            IF      ( LNL-ACC-CODE  =  '47' OR '48' OR '49' OR '57'    
"                                    OR '58' )                          
"                AND ( LNL-BC-COLM   =  '1' OR '2' )                    
"            THEN                                                       
"                      NEXT    SENTENCE                                 
"            ELSE                                                       
                       ADD     W-CTF-AMT    TO   W-LNP-APRV-UNUSED-BAL  
           END-IF.                                                      
           IF   (CTF-ACC-BUSINS-CODE  = 69)  AND  (LNL-BC-COLM = '9')
           THEN
             MOVE         SPACE       TO      LNL-BC-COLM.
830126     IF          LNP-C-ENT-HGHER-SCH-LN
890209          AND    LNM-C-ACC-LN-SCHOOL                              
           THEN
             IF        (CTF-RECV-TYPE      =       1)
             THEN
&&             ADD     1                   TO      LNM-RTRN-NO
             ELSE
               NEXT    SENTENCE
           ELSE
961206       MOVE      TWA-TXN-DAT7        TO      LNM-LAST-TXN-DATE    
             MOVE      0                   TO      LNM-BAL-SW.
           PERFORM     490221-CASE-BEG     THRU    490221-CASE-END.
       49022-EXIT.
           EXIT.
------*---------------依攤還方式更新分期攤還額、下次攤還日------------
       490221-CASE-BEG.
      *---------------收回1、中攤7會轉攤還方式為一般              
901109     IF        ( WKS-RTRN-TYPE   =  0 )                           
901109           AND ( BEF-RTRN-STYLE  =  1 )                           
           THEN                                                         
                       GO TO 490221-CASE-PRINCIPAL.                     
901109     IF        ( WKS-RTRN-TYPE   =  0 )                           
901109           AND ( BEF-RTRN-STYLE  =  2 )                           
           THEN                                                         
                       GO TO 490221-CASE-ANNUITY.                       
           IF         (WKS-C-GENERAL)       GO TO 490221-CASE-GENERAL.
           IF         (WKS-C-FIX-PRINCIPAL) GO TO 490221-CASE-PRINCIPAL.
           IF         (WKS-C-FIX-ANNUITY)   GO TO 490221-CASE-ANNUITY.
110509     IF         ( WKS-RTRN-TYPE = 7 ) GO TO 490221-CASE-PRINCIPAL.
           GO  TO     490221-CASE-ERR.
       490221-CASE-GENERAL.
           GO  TO      490221-CASE-END.
       490221-CASE-PRINCIPAL.
           IF          (LNL-LN-BAL  NOT =  0)
           THEN
830322       IF     (( LNP-C-ENT-HGHER-SCH-LN )                         
890209           AND ( LNM-C-ACC-LN-SCHOOL ))                           
             THEN
               MOVE    LNL-RECV-INT-DATE   TO      LNL-NEXT-RTRN-DATE
110509       ELSE IF ( WKS-RTRN-TYPE = 7 )                              
"                AND ( LNL-FIX-RTRN-CODE  =  '1' )                      
"                AND ( INT-P-PRNCPAL-THIS-INTVL > ZEROS )               
"              MOVE    INT-P-NX-RTRN-DATE  TO      LNL-NEXT-RTRN-DATE   
"            ELSE IF ( WKS-RTRN-TYPE = 7 )                              
"                AND ( LNL-FIX-RTRN-CODE  =  '1' )                      
"                AND ( INT-P-PRNCPAL-THIS-INTVL = ZEROS )               
"              NEXT    SENTENCE                                         
"            ELSE IF ( WKS-RTRN-TYPE = 7 )                              
"                AND ( LNL-FIX-RTRN-CODE  =  '0' )                      
"                AND ( CTF-RECV-AMT NOT = INT-P-PRNCPAL-THIS-INTVL )    
"                AND ( INT-P-PRNCPAL-THIS-INTVL > ZEROS )               
"              MOVE    INT-P-NX-RTRN-DATE  TO      LNL-NEXT-RTRN-DATE   
"              PERFORM 4902A-ECHR-CAL-RTN          THRU  4902A-EXIT     
"            ELSE IF ( WKS-RTRN-TYPE = 7 )                              
"                AND ( LNL-FIX-RTRN-CODE  =  '0' )                      
"                AND ( CTF-RECV-AMT > INT-P-PRNCPAL-THIS-INTVL )        
"                AND ( INT-P-PRNCPAL-THIS-INTVL = ZEROS )               
110509         PERFORM 4902A-ECHR-CAL-RTN          THRU  4902A-EXIT     
             ELSE
               PERFORM 9168-LN-RTRN-DATE-ADJ-RTN   W-QUOTIENT   TIMES.
           GO  TO      490221-CASE-END.
       490221-CASE-ANNUITY.
           IF          ((LNL-LN-BAL  NOT =  0)
                   AND  (NOT W-CTF-C-INSTMENT))
           THEN
             MOVE      LNL-RECV-INT-DATE   TO      LNL-NEXT-RTRN-DATE
            IF        ( LNL-AUTO-DEP-CODE-2  =  9 )                     
            THEN                                                        
                       MOVE  0         TO   LNL-AUTO-DEP-CODE-2         
            ELSE                                                        
901109        IF     ( WKS-RTRN-TYPE   =  2 )                           
              THEN                                                      
               MOVE      INT-P-ADJED-PER-RETURN-AMT
                                            TO      LNL-EACH-RTRN-AMT.
           GO  TO      490221-CASE-END.
       490221-CASE-ERR.
           MOVE        'S609'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490221-CASE-END.
       490221-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       49023-NML-FINAL-CHK.
           IF          (LNL-LN-BAL  NOT =  0)
                       GO  TO              49023-EXIT.
           IF          (LNL-RECV-RTRN-DIFF  NOT =  0)
           THEN
             MOVE      'W606'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           SUBTRACT    1                   FROM    LNP-LN-NO.
           PERFORM     490231-CASE-BEG     THRU    490231-CASE-END.
       49023-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       490231-CASE-BEG.
           IF          (W-CTF-C-RTRN)      GO  TO  490231-CASE-RTRN.
           IF          (W-CTF-C-RECV)      GO  TO  490231-CASE-RECV.
           IF          (W-CTF-C-TR)        GO  TO  490231-CASE-TR.
           IF          (W-CTF-C-INSTMENT)  GO  TO  490231-CASE-INSTMENT.
           IF          (W-CTF-C-TR-OVER)   GO  TO  490231-CASE-TR-OVER.
           IF          (W-CTF-C-MVOUT)     GO  TO  490231-CASE-MVOUT.
890922     IF          (W-CTF-C-MID-RTRN)  GO  TO  490231-CASE-END.     
           GO  TO      490231-CASE-ERR.
       490231-CASE-RTRN.
           MOVE        LNL-M-RTRN          TO      LNL-STATUS.
           GO  TO      490231-CASE-END.
       490231-CASE-RECV.
0606XX     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"            IF          (CTF-RECV-AMT  NOT <  LNL-LN-BAL)              
"            THEN                                                       
"              GO  TO      490231-CASE-END.                             
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490231-CASE-END.
       490231-CASE-TR.
           MOVE        LNL-M-TR            TO      LNL-STATUS.
           GO  TO      490231-CASE-END.
       490231-CASE-INSTMENT.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490231-CASE-END.
       490231-CASE-TR-OVER.
920513     MOVE        WS-BEF-47-INT       TO      LNL-INT-RATE.        
           MOVE        LNL-M-TR-OVER       TO      LNL-STATUS.
           GO  TO      490231-CASE-END.
       490231-CASE-MVOUT.
           MOVE        LNL-M-MVOUT         TO      LNL-STATUS.
           GO  TO      490231-CASE-END.
       490231-CASE-ERR.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490231-CASE-END.
       490231-CASE-END.
           EXIT.
------*---------------更新主檔ＬＮＬ、ＬＮＰ、ＬＮＬ------------------
       49024-WRITE-BACK.
920527*---------------分段優惠利率處理                                
891019     PERFORM     9251-READ-RATE      THRU     9251-EXIT.          
           IF        ( NOT  TWA-C-VSAM-NOTFND )                         
               AND   ( LN-RATE-NORMAL )                                 
               AND   ( TWA-C-TXN-NOT-REVERSE )                          
920924         AND   ( CTF-RECV-TYPE  =  '2' OR '4' OR '7' )            
           THEN                                                         
             PERFORM   499600-SEAR-END-DATE-RTN   THRU  499600-EXIT     
             UNTIL     END-RATE-INDEX    >  4                           
             PERFORM   499601-SEAR-PRE-RATE-RTN   THRU  499601-EXIT     
             UNTIL     CHG-RATE-INDEX    >  4  OR  CHG-SW-Y             
             IF        CHG-SW-Y                                         
             THEN                                                       
                       PERFORM 499602-SEAR-INT-RATE-RTN THRU 499602-EXIT
                       PERFORM 499603-COMP-INT-RATE-RTN THRU 499603-EXIT
920527                 PERFORM 499604-UPD-ADJ-DATA-RTN THRU 499604-EXIT.
      *---------------逾最後截止日：狀況改為９                        
           MOVE        TWA-M-VSAM-NORMAL   TO      TWA-VSAM-RETURN-CODE.
921121     IF          CTF-INT-END-DATE-N-R  NOT < WKS-END-RATE-DATE    
           THEN                                                         
891019                 PERFORM 9252-UPDE-RATE      THRU     9252-EXIT.  
891019     IF          (TWA-C-TXN-REVERSE)                              
           THEN                                                         
891019     MOVE        REV-INCR-ALNRATE    TO    LNL-INT-INCR-DECR-RATE.
920903*---------------收回原因                                        
101025     IF          (TWA-C-TXN-REVERSE)                              
"          THEN                                                         
"              MOVE    REV-LNL-RTRN-RSN     TO   LNL-RTRN-RSN           
101025     ELSE                                                         
971222         MOVE    CTF-CAPTL-PURPOSE    TO   LNL-RTRN-RSN.          
      *    ENDIF                                                        
980720*---------------免責債權種類                                    
980720     IF          CTF-CAPTL-PURPOSE    =    '90'                   
"          THEN                                                         
"            IF        LNL-LN-BAL      NOT  >    0                      
"            THEN                                                       
"              MOVE    CTF-RELIEF-TYPE      TO   LNL-RELIEF-TYPE        
"            ELSE                                                       
"              MOVE    '00'                 TO   LNL-RELIEF-TYPE.       
980513*---------------固定攤還額，攤還額寫回                          
           IF          LNL-C-FIX-ON                                     
               AND     (TWA-C-TXN-NOT-REVERSE)                          
           THEN                                                         
980513                 MOVE  BEF-EACH-RTRN-AMT  TO   LNL-EACH-RTRN-AMT. 
920321*---------------微調提存利息                                    
"          IF        ( TWA-C-LN-ADJ-INT )                               
"          THEN                                                         
920321                 PERFORM 9220-RVBL-INT-PROC-RTN  THRU  9220-EXIT. 
      *    ENDIF                                                        
040324*--------公教國宅拍賣後採按日計息－只還本金；收回時收息         
040324     IF        ( LNM-C-ACC-LN-NATION       )                      
                AND  ( LNP-CT-INTVL    =    '01' )                      
  "        THEN                                                         
"                      MOVE  BEF-RTRN-CODE      TO  LNP-RTRN-CODE       
"                      MOVE  BEF-EACH-RTRN-AMT  TO  LNL-EACH-RTRN-AMT.  
      *    ENDIF                                                        
           PERFORM     9060-LN-DBS-LF-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           IF          CTF-APRV-SER-SUB-NO         =  ZEROS
           THEN
             MOVE      W-LNP-APRV-UNUSED-BAL    TO LNP-APRV-UNUSED-BAL.
      *    ENDIF                                                        
030224     IF          LNP-LN-NO  <  ZEROS
           THEN
                       MOVE  +0     TO  LNP-LN-NO.
           PERFORM     9032-LN-DBS-PF-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
      *---------------更新總號ＬＮＰ〔未動用額度〕                    
           IF          CTF-APRV-SER-SUB-NO  NOT    =  ZEROS
           THEN
             MOVE      WLNP                       TO LNP
             MOVE      W-LNP-APRV-UNUSED-BAL    TO LNP-APRV-UNUSED-BAL
             PERFORM   9032-LN-DBS-PFT-RTN
             PERFORM   9105-LN-DBS-ERR-HDL-RTN.
      *    ENDIF                                                        
021210*---------------處理ＬＮＭ〔歸戶餘額〕                          
"          IF        ( LNM-TOT-BAL  NOT NUMERIC )                       
"                      MOVE    ZEROS             TO   LNM-TOT-BAL       
"          ELSE IF   ( TWA-C-TXN-REVERSE )                              
"                      COMPUTE LNM-TOT-BAL  =    LNM-TOT-BAL            
"                                             +  WK-LNM-TXN-AMT         
"          ELSE IF   ( LNM-TOT-BAL  >  WK-LNM-TXN-AMT )                 
"                      COMPUTE LNM-TOT-BAL  =    LNM-TOT-BAL            
"                                             -  WK-LNM-TXN-AMT         
"          ELSE                                                         
"                      MOVE    ZEROS             TO   LNM-TOT-BAL.      
"     *    ENDIF                                                        
061206*---------處理就學貸款簡易轉呆帳
           IF          WK-SIMP-CODE         =    'S'
           THEN
                       MOVE  'S'     TO  LNM-SCH-TMP.
      *---------------更新ＬＮＭ                                      
           PERFORM     9020-LN-DBS-MF-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
       49024-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49025-TERM-COMPUTE-RTN.
           PERFORM     9171-LN-INT-AREA-CLEAR.
A91114     MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE-DATE.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.
900706     MOVE        CTF-INT-END-DATE-N-R TO     DAT-P-BEG-DATE-R.    
           MOVE        WKS-DUE-DATE        TO      DAT-P-END-DATE.
           MOVE        LNP-RTRN-INTVL      TO      DAT-P-TERM-UNIT.
           IF          LNP-C-RTRN-MONTH-6
           THEN
             MOVE      06                  TO      DAT-P-TERM-UNIT.
           MOVE        DAT-M-FUN-5         TO      DAT-P-FUN-CODE.
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
       49025-EXIT.
           EXIT.
------*---------------攤還本金：檢驗金額起訖日、計息------------------
       4903-INT-NML-P.
           PERFORM     49030-CASE-BEG      THRU    49030-CASE-END.
       4903-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       49030-CASE-BEG.
961206     IF          (LNL-L-RECV-INT-DATE-R  >  TWA-TXN-DAT7)         
                       GO  TO              49030-CASE-GT.
961206     IF          (LNL-L-RECV-INT-DATE-R  =  TWA-TXN-DAT7)         
                       GO  TO              49030-CASE-EQ.
           GO  TO      49030-CASE-LT.
       49030-CASE-GT.
           IF          (W-CTF-C-RECV)
           THEN
             PERFORM   49032-PREPAY-INT    THRU    49032-EXIT
           ELSE
             MOVE      'W612'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN
             PERFORM   49011-CHK-INT-FINE  THRU    49011-EXIT.
           GO  TO      49030-CASE-END.
       49030-CASE-EQ.
           IF          (W-CTF-C-RECV)
           THEN
             PERFORM   49032-PREPAY-INT    THRU    49032-EXIT
           ELSE
             PERFORM   49011-CHK-INT-FINE  THRU    49011-EXIT.
           GO  TO      49030-CASE-END.
       49030-CASE-LT.
970609     IF      (CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R) 
           THEN
             MOVE      'M643'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49031-CHK-INT-END-DATE-P    THRU    49031-EXIT.
961206     IF          (TWA-TXN-DAT7    NOT >  WKS-DUE-DATE)            
           THEN
961206       IF        ((TWA-TXN-DAT7    NOT >  LNL-RECV-INT-DATE-R)    
                   AND  (CTF-FINE  NOT =  0))
             THEN
               MOVE      'M660'              TO      MSG-P-OUT-CODE
               PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49012-CALCULATE-INT THRU    49012-EXIT.
           PERFORM     49013-ESTB-NML-LNI  THRU    49013-EXIT.
050122*---------------排除〔呆帳〕                                    
"          IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  49030-CASE-END.                          
"     *    ENDIF                                                        
      *---------------處理違約ＬＮＩ                                  
           IF          ((WS-INT-FINE   NOT =  0)
                   OR   (CTF-FINE NOT =  0))
           THEN
               PERFORM 9145-LN-ESTB-FINE-LNI-RTN
               MOVE    1                   TO      W-FINE-SW.
           GO  TO      49030-CASE-END.
       49030-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       49031-CHK-INT-END-DATE-P.
           PERFORM     490311-CASE-BEG     THRU    490311-CASE-END.
       49031-EXIT.
           EXIT.
       490311-CASE-BEG.
           IF          (W-CTF-C-RTRN
                    OR  W-CTF-C-TR)        GO  TO  490311-CASE-RTRN-TR.
           IF          (W-CTF-C-RECV)      GO  TO  490311-CASE-RECV.
           IF          (W-CTF-C-INSTMENT
                    OR  W-CTF-C-TR-OVER
                    OR  W-CTF-C-MVOUT)     GO  TO  490311-CASE-OTHERS.
           GO  TO      490311-CASE-ERR.
       490311-CASE-RTRN-TR.
821224     IF           LNP-C-ENT-HGHER-SCH-LN
890209          AND     LNM-C-ACC-LN-SCHOOL                             
           THEN
             GO  TO     490311-CASE-END.
961206     IF          (CTF-INT-END-DATE-N-R  NOT =  TWA-TXN-DAT7)      
           THEN
             MOVE      'M644'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490311-CASE-END.
       490311-CASE-RECV.
           IF          (CTF-ACC-BUSINS-CODE  =  69)
                  AND  (LNP-C-CTZN-HOUSE-LN
                    OR  LNP-C-LABOR-HOUSE-LN
                    OR  LNP-C-CTZN-MATCH-LN
                    OR  LNP-C-GVR-HOUSE-LN)
           THEN
           PERFORM     490512-CTF-END-DATE-CHECK  THRU  490512-END-EXIT.
           GO  TO      490311-CASE-END.
       490311-CASE-OTHERS.
           GO  TO      490311-CASE-END.
       490311-CASE-ERR.
           MOVE        'M686'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490311-CASE-END.
       490311-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       49032-PREPAY-INT.
           IF          (CTF-FINE  NOT =  0)
           THEN
             MOVE      'M660'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
970609     IF     (CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R)  
           THEN
             MOVE      'M643'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49012-CALCULATE-INT         THRU    49012-EXIT.
           PERFORM     49013-ESTB-NML-LNI          THRU    49013-EXIT.
       49032-EXIT.
           EXIT.
------*---------------更新主檔（攤還本金）----------------------------
       4904-UPDATE-NML-P.
           IF          (NOT W-CTF-C-INSTMENT)
           THEN
961206       IF        (((LNL-L-RECV-INT-DATE-R  <  TWA-TXN-DAT7)       
                    OR   (W-CTF-C-RECV))
970609             AND  (CTF-INT-BGN-DATE-N-R  NOT =  ZERO))            
             THEN
               PERFORM 49021-UPDATE-NML-INT        THRU    49021-EXIT.
           PERFORM     49022-UPDATE-NML-RTRN       THRU    49022-EXIT.
           PERFORM     49023-NML-FINAL-CHK         THRU    49023-EXIT.
           PERFORM     49024-WRITE-BACK            THRU    49024-EXIT.
       4904-EXIT.
           EXIT.
------*---------------攤還本息：檢驗金額起訖日、計息------------------
       4905-INT-NML-A.
           IF          ((W-CTF-C-RTRN
                   OR    W-CTF-C-TR)
961206             AND  (LNL-L-RECV-INT-DATE-R NOT < TWA-TXN-DAT7))     
901109           AND ( WK-LABOR-DELAY-INT  =  ZEROS )                   
           THEN                                                         
                       GO  TO              4905-EXIT.
970609     IF      (CTF-INT-BGN-DATE-N-R  NOT =  LNL-L-RECV-INT-DATE-R) 
           THEN
             MOVE      'M643'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49051-CHK-INT-END-DATE-A    THRU    49051-EXIT.
961206     IF          (TWA-TXN-DAT7  NOT >  WKS-DUE-DATE)              
           THEN
961206       IF        ((TWA-TXN-DAT7  NOT >  LNL-RECV-INT-DATE-R)      
                  AND  (CTF-FINE  NOT =  0))
             THEN
               MOVE      'M660'              TO      MSG-P-OUT-CODE
               PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49012-CALCULATE-INT THRU    49012-EXIT.
           PERFORM     49013-ESTB-NML-LNI  THRU    49013-EXIT.
050122*---------------排除〔呆帳〕                                    
0606XX     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  4905-EXIT.                               
"     *    ENDIF                                                        
      *---------------處理違約ＬＮＩ                                  
           IF          ((WS-INT-FINE =  0)
                  AND  (CTF-FINE =  0))    GO  TO  4905-EXIT.
           PERFORM     9145-LN-ESTB-FINE-LNI-RTN.                       
           MOVE        1                   TO      W-FINE-SW.           
       4905-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49051-CHK-INT-END-DATE-A.
           PERFORM     490511-CASE-BEG     THRU    490511-CASE-END.
       49051-EXIT.
           EXIT.
       490511-CASE-BEG.
           IF          (W-CTF-C-RTRN
                    OR  W-CTF-C-TR)        GO  TO  490511-CASE-RTRN-TR.
           IF          (W-CTF-C-RECV)      GO  TO  490511-CASE-RECV.
           IF          (W-CTF-C-INSTMENT
                    OR  W-CTF-C-TR-OVER
                    OR  W-CTF-C-MVOUT)     GO  TO  490511-CASE-OTHERS.
           GO  TO      490511-CASE-ERR.
       490511-CASE-RTRN-TR.
961206     IF          (CTF-INT-END-DATE-N-R  NOT =  TWA-TXN-DAT7)      
           THEN
             MOVE      'M644'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490511-CASE-END.
       490511-CASE-RECV.
           PERFORM     490512-CTF-END-DATE-CHECK  THRU  490512-END-EXIT.
           GO  TO      490511-CASE-END.
       490511-CASE-OTHERS.
           GO  TO      490511-CASE-END.
       490511-CASE-ERR.
           MOVE        'M686'              TO      MSG-P-OUT-CODE
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      490511-CASE-END.
       490511-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       490512-CTF-END-DATE-CHECK.
           IF        (LNP-C-RTRN-NORMAL)
           THEN
             MOVE      'S609'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
900706     IF          CTF-INT-END-DATE-N-R   <   LNL-RECV-INT-DATE-R   
           THEN
             MOVE      'M644'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
900706     IF          CTF-INT-END-DATE-N-R =   LNL-RECV-INT-DATE-R     
           THEN
             GO  TO    490512-END-EXIT.
040317*---------------計算期數＆相當日                                
"          MOVE        LNL-RECV-INT-DATE    TO   WK-DAT-BEG-DATE.       
"          MOVE        CTF-INT-END-DATE-N-R TO   WK-DAT-END-DATE        
"          MOVE        LNL-RECV-INT-DAY     TO   WK-DAT-DAYS.           
"          MOVE        LNP-RTRN-INTVL       TO   WK-DAT-TERM-UNIT.      
"          IF        ( LNP-C-RTRN-MONTH-1 )                             
"                      MOVE    01                TO   WK-DAT-TERM-UNIT  
"          ELSE IF   ( LNP-C-RTRN-MONTH-6 )                             
"                      MOVE    06                TO   WK-DAT-TERM-UNIT  
"          ELSE                                                         
"                      MOVE    01                TO   WK-DAT-TERM-UNIT. 
"     *    ENDIF                                                        
"          PERFORM     815-DAT-15-CAL-RTN        THRU  815-EXIT.        
900706     IF     CTF-INT-END-DATE-N-R NOT =    DAT-P-CORR-DATE-R       
           THEN
             MOVE    'M644'                TO      MSG-P-OUT-CODE
             PERFORM 9146-LN-ERR-MSG-OUT-RTN.
       490512-END-EXIT.
            EXIT.
------*-----------------------------------------------------------------
       4906-UPDATE-NML-A.
           IF          (NOT W-CTF-C-INSTMENT)
           THEN
             IF        (((W-CTF-C-RTRN  OR  W-CTF-C-TR)
961206             AND (LNL-L-RECV-INT-DATE-R  NOT <  TWA-TXN-DAT7))    
"                   OR  (CTF-INT-BGN-DATE-N-R  =  ZERO))                
             THEN
               NEXT    SENTENCE
             ELSE
               PERFORM 49021-UPDATE-NML-INT        THRU    49021-EXIT.
           PERFORM     49022-UPDATE-NML-RTRN       THRU    49022-EXIT.
           PERFORM     49023-NML-FINAL-CHK         THRU    49023-EXIT.
           PERFORM     49024-WRITE-BACK            THRU    49024-EXIT.
       4906-EXIT.
           EXIT.
------*---------------ＬＯＧ交易後欄位處理----------------------------
       4907-PREPARE-AFT.
           IF          ((LNL-C-RTRN)
                   AND  (LNL-RECV-RTRN-DIFF  =  0))
           THEN
             MOVE      '**'                TO      AFT-CLOSED-MARK
           ELSE
             MOVE      SPACE               TO      AFT-CLOSED-MARK.
971222     MOVE        CTF-CAPTL-PURPOSE    TO   AFT-AMND-CODE.         
           MOVE        LNL-ACC-CODE        TO      AFT-ACC-CODE.
           MOVE        LNL-CURRENCY-CODE   TO      AFT-CURRENCY-CODE.
           MOVE        LNL-RECV-RTRN-DIFF  TO      AFT-RECV-RTRN-DIFF.
           MOVE        CTF-RECV-AMT        TO      AFT-RECVED-AMT.
           MOVE        LNL-INT-RATE        TO      AFT-INT-RATE.
971212     MOVE        LNL-1ST-LN-DATE-R   TO      AFT-1ST-LN-DATE.     
           MOVE        LNL-DUE-DATE        TO      AFT-DUE-DATE.
           MOVE        BEF-LN-BAL          TO      AFT-FOR-INT-AMT.
900914     MOVE        LNM-MAIN-BUSINS-CODE TO   AFT-MAIN-BUSINS-CODE.  
           IF          (LNL-C-AR-OVERDUE
                    OR  LNL-C-ACC-RECVABL)
           THEN
             PERFORM   4907-0-PREPARE-AFT  THRU    4907-0-EXIT
             MOVE      LNP-APRV-AMT        TO      AFT-FOR-FINE-AMT
             GO  TO    4907-EXIT.
001019     IF        ( CLNP-RECV-INT-TYPE  =  '01' )                    
           THEN
             PERFORM   4907-1-PREPARE-AFT  THRU    4907-1-EXIT
             MOVE      LNP-APRV-AMT        TO      AFT-FOR-FINE-AMT
             GO  TO    4907-EXIT.
           PERFORM     49071-CASE-BEG      THRU    49071-CASE-END.
           MOVE        LNP-APRV-AMT        TO      AFT-FOR-FINE-AMT.
       4907-EXIT.
           EXIT.
------*---------------ＬＯＧ交易後（催收、應收帳款）------------------
       4907-0-PREPARE-AFT.
           PERFORM     49071-0-CASE-BEG    THRU    49071-0-CASE-END.
       4907-0-EXIT.
           EXIT.
------*---------------ＬＯＧ交易後（不計息戶）------------------------
       4907-1-PREPARE-AFT.
           PERFORM     49071-1-CASE-BEG    THRU    49071-1-CASE-END.
       4907-1-EXIT.
           EXIT.
------*---------------ＬＯＧ交易後欄位--------------------------------
       49071-CASE-BEG.
           IF          (W-CTF-C-RTRN
                    OR  W-CTF-C-TR)        GO  TO  49071-CASE-RTRN-TR.
           IF          (W-CTF-C-RECV)      GO  TO  49071-CASE-RECV.
           IF          (W-CTF-C-INSTMENT)  GO  TO  49071-CASE-INSTMENT.
           IF          (W-CTF-C-TR-OVER)   GO  TO  49071-CASE-TR-OVER.
           IF          (W-CTF-C-MVOUT)     GO  TO  49071-CASE-MVOUT.
890922     IF          (W-CTF-C-MID-RTRN)  GO  TO  49071-CASE-INSTMENT. 
090224     IF          (W-CTF-C-SCH-RTRN)  GO  TO  49071-CASE-RECV.     
           GO  TO      49071-CASE-ERR.
       49071-CASE-RTRN-TR.
961206     IF          ((BEF-L-RECV-INT-DATE  NOT <  TWA-TXN-DAT7)      
"                   OR  (CTF-INT-BGN-DATE-N-R  =  ZERO))                
                       GO  TO              49071-CASE-END.
           PERFORM     490711-PREPARE-AFT-INT      THRU    490711-EXIT.
           IF          (W-FINE-SW  =  0)   GO  TO  49071-CASE-END.
           PERFORM     490712-PREPARE-AFT-FINE     THRU    490712-EXIT.
           GO  TO      49071-CASE-END.
       49071-CASE-RECV.
           PERFORM     490711-PREPARE-AFT-INT      THRU    490711-EXIT.
           IF          (W-FINE-SW  =  0)   GO  TO  49071-CASE-END.
           PERFORM     490712-PREPARE-AFT-FINE     THRU    490712-EXIT.
           GO  TO      49071-CASE-END.
       49071-CASE-INSTMENT.
           GO  TO      49071-CASE-END.
       49071-CASE-TR-OVER.
           IF          (CTF-INT-AMT = 0)   GO  TO  49071-CASE-END.
           PERFORM     490711-PREPARE-AFT-INT      THRU    490711-EXIT.
           IF          (W-FINE-SW  =  0)   GO  TO  49071-CASE-END.
           PERFORM     490712-PREPARE-AFT-FINE     THRU    490712-EXIT.
           GO  TO      49071-CASE-END.
       49071-CASE-MVOUT.
           GO  TO      49071-CASE-END.
       49071-CASE-ERR.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49071-CASE-END.
       49071-CASE-END.
           EXIT.
------*---------------ＬＯＧ交易後（催收、應收帳款）------------------
       49071-0-CASE-BEG.
           IF          (W-CTF-C-RTRN
                    OR  W-CTF-C-RECV
090122              OR  W-CTF-C-SCH-RTRN                                
                    OR  W-CTF-C-TR-OVER) GO TO 49071-0-CASE-RTRN-RECV.
           IF          (W-CTF-C-TR
                    OR  W-CTF-C-INSTMENT
                    OR  W-CTF-C-MVOUT) GO  TO  49071-0-CASE-OTHERS.
           GO  TO      49071-0-CASE-ERR.
       49071-0-CASE-RTRN-RECV.
           IF          (CTF-INT-AMT  =  0) GO  TO  49071-0-CASE-END.
           PERFORM     490711-PREPARE-AFT-INT      THRU    490711-EXIT.
           IF          (W-FINE-SW  =  0)   GO  TO  49071-0-CASE-END.
           PERFORM     490712-PREPARE-AFT-FINE     THRU    490712-EXIT.
           GO  TO      49071-0-CASE-END.
       49071-0-CASE-OTHERS.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49071-0-CASE-END.
       49071-0-CASE-ERR.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49071-0-CASE-END.
       49071-0-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       49071-1-CASE-BEG.
           IF          (W-CTF-C-RTRN
                    OR  W-CTF-C-RECV
                    OR  W-CTF-C-TR
                    OR  W-CTF-C-TR-OVER
                    OR  W-CTF-C-MVOUT)    GO  TO  49071-1-CASE-OK.
           IF          (W-CTF-C-INSTMENT) GO  TO  49071-1-CASE-OTHERS.
           GO  TO      49071-1-CASE-ERR.
       49071-1-CASE-OK.
           IF          (CTF-INT-AMT  =  0) GO  TO  49071-1-CASE-END.
           PERFORM     490711-PREPARE-AFT-INT      THRU    490711-EXIT.
           IF          (W-FINE-SW  =  0)   GO  TO  49071-1-CASE-END.
           PERFORM     490712-PREPARE-AFT-FINE     THRU    490712-EXIT.
           GO  TO      49071-1-CASE-END.
       49071-1-CASE-OTHERS.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49071-1-CASE-END.
       49071-1-CASE-ERR.
           MOVE        'D603'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      49071-1-CASE-END.
       49071-1-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       490711-PREPARE-AFT-INT.
           MOVE        WSI-INT-RATE        TO      AFT-NML-INT-RATE.
           MOVE        WSI-INT-BGN-DATE    TO      AFT-INT-BGN-DATE.
           MOVE        WSI-INT-END-DATE    TO      AFT-INT-END-DATE.
           MOVE        WSI-DAYS            TO      AFT-NML-DAYS.
           MOVE        WSI-RECVABL-AMT     TO      AFT-RECVABL-INT-AMT.
       490711-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       490712-PREPARE-AFT-FINE.
           MOVE        LNI-INT-RATE        TO      AFT-FINE-INT-RATE.
970922     MOVE        LNI-INT-BGN-DATE-R  TO      AFT-FINE-BGN-DATE.   
"          MOVE        LNI-INT-END-DATE-R  TO      AFT-FINE-END-DATE.   
           MOVE        LNI-DAYS            TO      AFT-FINE-DAYS.
           MOVE        LNI-RECVABL-AMT     TO      AFT-RECVABL-FINE-AMT.
       490712-EXIT.
           EXIT.
------*---------------沖正：處理ＬＮＩ--------------------------------
       4908-INT-RVS.
050122*---------------排除〔呆帳〕                                    
0606XX     IF        ( LNL-ACC-CODE  =  '48' OR '49' OR '58' )          
"          THEN                                                         
"                      GO  TO  4908-EXIT.                               
"     *    ENDIF                                                        
           IF        ( LNP-C-INT-FREE OR LNP-C-RECV-CMS )               
           THEN                                                         
                       GO TO 4908-EXIT.                                 
      *---------------讀原ＬＮＩ                                      
           PERFORM     9091-LN-DBS-I2-RTN.
           PERFORM     9106-LN-DBS-ERR-HDL-RTN-BOTH.
           IF          TWA-C-DBS-NO-REC-FOUND      GO TO  4908-EXIT.
950125*---------------貸款類別４４改為「批次信用保證貸款」            
900706     IF        (((LNI-INT-END-DATE-R  NOT =  CTF-INT-END-DATE-N-R)
791114              AND (LNI-INT-END-DATE-R NOT = REV-RECV-INT-DATE))
                    OR  (NOT  LNI-C-NORMAL)
                    OR  (NOT  LNI-C-INT-NORMAL)
                    OR  (CTF-INT-AMT  NOT =  LNI-RECVED-AMT))
           THEN
             MOVE      0                   TO      W-SWITCH
             PERFORM   49081-GET-NML-INT   THRU    49081-EXIT
               UNTIL   (W-SWITCH  =  1).
77527      IF       NOT    LNL-C-TR-OVER
911101        AND                                                       
961206        ( NOT (( REV-L-RECV-INT-DATE < TWA-TXN-DAT7)              
"                AND ( LNP-C-ENT-HGHER-SCH-LN )                         
990125           AND  NOT ( LNP-C-SCH-GEN )                             
"                AND ( LNP-RTRN-CODE            =  '3' )                
911101           AND ( NOT   LNL-C-AR-OVERDUE )))                       
           THEN
791114       IF      (LNI-INT-END-DATE-R  >  LNL-L-RECV-INT-DATE-R)
             THEN
               MOVE      'S610'              TO      MSG-P-OUT-CODE
               PERFORM   9146-LN-ERR-MSG-OUT-RTN
             ELSE
791114         IF     ((LNI-INT-END-DATE-R  <  LNL-L-RECV-INT-DATE-R)
  "               AND  (LNI-INT-END-DATE-R NOT = REV-RECV-INT-DATE))
               THEN
                 MOVE    'M617'              TO      MSG-P-OUT-CODE
                 PERFORM 9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49082-RVS-INT-REC           THRU    49082-EXIT.
           PERFORM     49083-FINE-OTHER-INT-CHK    THRU    49083-EXIT.
       4908-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49081-GET-NML-INT.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNI        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNI.         
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
900706     IF          (((LNI-INT-END-DATE-R  =  CTF-INT-END-DATE-N-R)  
791114             OR    (LNI-INT-END-DATE-R  =  REV-RECV-INT-DATE))
                   AND  (LNI-C-NORMAL)
                   AND  (LNI-C-INT-NORMAL)
                   AND  (CTF-INT-AMT  =  LNI-RECVED-AMT))
           THEN
             MOVE      1                   TO      W-SWITCH.
       49081-EXIT.
           EXIT.
------*---------------沖正：處理ＬＮＩ--------------------------------
       49082-RVS-INT-REC.
           MOVE        LNI-M-ERR-DLT       TO      LNI-STATUS.
991203     IF     ( NOT LNI-C-EIR-REC )                                 
           COMPUTE     W-DIFF =  W-DIFF +  LNI-RECVABL-AMT -
                                 LNI-RECVED-AMT.
           IF          (LNI-C-NORMAL)
991203     AND    ( NOT LNI-C-EIR-REC )                                 
           THEN
             MOVE      LNI-RECVABL-AMT     TO      WS-INT.
           PERFORM     9094-LN-DBS-I5-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
991203     IF     ( LNI-C-EIR-REC )                                     
991203     THEN                                                         
991203       GO    TO   49082-EXIT.                                     
           ADD         1                   TO      W-LNI-NO.
9911       IF          (LNI-C-NORMAL)                                   
           THEN
             MOVE      LNI-RECVABL-AMT     TO      AFT-RECVABL-INT-AMT
           ELSE
             MOVE      LNI-RECVABL-AMT     TO      AFT-RECVABL-FINE-AMT.
       49082-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49083-FINE-OTHER-INT-CHK.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490831-GET-FINE-OTHER-INT   THRU    490831-EXIT
             UNTIL     (W-SWITCH  NOT = 0).
           IF          (W-SWITCH = 2)      GO  TO  49083-EXIT.
           IF          ((W-SWITCH = 1)
991203             AND  (NOT LNI-C-FINE-OVER)                           
991203             AND  (NOT LNI-C-EIR-REC))                            
           THEN
             MOVE      'Z601'              TO      MSG-P-OUT-CODE       
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     49082-RVS-INT-REC   THRU    49082-EXIT.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490832-GET-OTHER-INT        THRU    490832-EXIT
             UNTIL     (W-SWITCH = 2).
       49083-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       490831-GET-FINE-OTHER-INT.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNI        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNI.         
           PERFORM     9106-LN-DBS-ERR-HDL-RTN-BOTH.
           IF          (TWA-C-DBS-NO-REC-FOUND)
           THEN
             MOVE      2                   TO      W-SWITCH
           ELSE
             IF        (LNI-C-NORMAL)
             THEN
               MOVE    1                   TO      W-SWITCH.
       490831-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       490832-GET-OTHER-INT.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNI        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNI.         
           PERFORM     9106-LN-DBS-ERR-HDL-RTN-BOTH.
           IF          (TWA-C-DBS-NO-REC-FOUND)
           THEN
             MOVE      2                   TO      W-SWITCH
991203     ELSE  IF ( LNI-C-EIR-REC )                                   
991203     THEN                                                         
991203       PERFORM     49082-RVS-INT-REC   THRU    49082-EXIT         
991203     ELSE IF     (LNI-C-NORMAL)                                   
             THEN
               MOVE    'Z601'              TO      MSG-P-OUT-CODE       
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
       490832-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49084-REV-FINE-ONLY.
           IF          ((LNI-C-NORMAL)
                   AND  (LNI-C-FINE-OVER)
                   AND  (CTF-FINE  =  LNI-RECVED-AMT))
           THEN
             PERFORM   49082-RVS-INT-REC   THRU    49082-EXIT
             MOVE      0                   TO      W-SWITCH
             PERFORM   490832-GET-OTHER-INT        THRU    490832-EXIT
               UNTIL   (W-SWITCH = 2)
             GO  TO    49084-EXIT.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490841-GET-FINE     THRU    490841-EXIT
             UNTIL     (W-SWITCH = 1).
           PERFORM     49082-RVS-INT-REC   THRU    49082-EXIT.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     490832-GET-OTHER-INT        THRU    490832-EXIT
             UNTIL     (W-SWITCH = 2).
       49084-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       490841-GET-FINE.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNI        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNI.         
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           IF          ((LNI-C-NORMAL)
                   AND  (LNI-C-FINE-OVER)
                   AND  (CTF-FINE  =  LNI-RECVED-AMT))
           THEN
             MOVE      1                   TO      W-SWITCH.
       490841-EXIT.
           EXIT.
------*---------------沖正：處理ＬＮＲ--------------------------------
       4909-RTRN-RVS.
140731*---------------計算：原交易之攤還後餘額                        
"          COMPUTE     W-RTRN-BAL  =  REV-LN-BAL  -  CTF-RECV-AMT.      
141209*---------------比對ＬＮＬ餘額（轉催／轉籍）                    
"          IF        ( CTF-RECV-TYPE =  '5' OR '6' )                    
"                AND ( LNL-LN-BAL    =  REV-LN-BAL )                    
"                      NEXT    SENTENCE                                 
141209*---------------比對ＬＮＬ餘額（其他類別）                      
"          ELSE IF   ( CTF-RECV-TYPE NOT = '5' )                        
"                AND ( CTF-RECV-TYPE NOT = '6' )                        
"                AND ( LNL-LN-BAL    =  W-RTRN-BAL )                    
"                      NEXT    SENTENCE                                 
"          ELSE                                                         
140731                 MOVE    'S668'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN                  
"          END-IF.                                                      
110509*---------------攤還本金＝０（無ＬＮＲ）：不處理ＬＮＲ          
"          IF        ( CTF-RECV-AMT  =  ZEROS )                         
"          THEN                                                         
"                      GO    TO   4909-EXIT.                            
110509*    ENDIF                                                        
           PERFORM     9076-LN-DBS-R2-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
951212*---------------累計：應補收退金額                              
           COMPUTE     W-RTRN-BAL  =  REV-LN-BAL  -  CTF-RECV-AMT.
950612     IF        ( LNR-RTRN-BAL    NOT =  W-RTRN-BAL )              
"                 OR ( LNR-RTRN-AMT    NOT =  CTF-RECV-AMT )            
"                 OR ( NOT LNR-C-NORMAL )                               
           THEN
             MOVE      0                   TO      W-SWITCH
             PERFORM   49091-GET-RTRN      THRU    49091-EXIT
               UNTIL   (W-SWITCH = 1).
           IF          (CTF-RECV-AMT  NOT =  LNR-RTRN-AMT)
           THEN
             MOVE      'S612'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
951212*---------------累計：應補收退金額                              
"          IF        ( LNR-RTRNABL-AMT  NUMERIC )                       
"               AND  ( LNR-RTRNABL-AMT  NOT =  ZEROS )                  
"          THEN                                                         
"                      COMPUTE W-DIFF  =    W-DIFF                      
"                                        +  LNR-RTRNABL-AMT             
"                                        -  LNR-RTRN-AMT.               
           MOVE        LNR-M-ERR-DLT       TO      LNR-STATUS.
           PERFORM     9079-LN-DBS-R5-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           ADD         1                   TO      W-LNR-NO.
           MOVE        0                   TO      W-SWITCH.
           PERFORM     49092-GET-OTHER-RTRN        THRU    49092-EXIT
             UNTIL     (W-SWITCH = 2).
       4909-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49091-GET-RTRN.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNR        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNR.         
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
950612     IF        ( LNR-RTRN-BAL  =  W-RTRN-BAL )                    
"                AND ( LNR-RTRN-AMT  =  CTF-RECV-AMT )                  
"                AND ( LNR-C-NORMAL )                                   
           THEN
             MOVE      1                   TO      W-SWITCH.
       49091-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       49092-GET-OTHER-RTRN.
"          MOVE        TWA-M-LN            TO      TWA-DBS-FILE-ID.     
970922     MOVE        TWA-TXN-DAT7        TO      TWA-DBS-SEG-KEY.     
"          MOVE        TWA-M-LN-LNR        TO      TWA-DBS-SEG-NAME.    
"          MOVE        TWA-M-READ-NEXT-UPDATE-S TO TWA-DBS-FUN-CODE.    
"          CALL        'UCPDBS'            USING   TWA     LNR.         
           PERFORM     9106-LN-DBS-ERR-HDL-RTN-BOTH.
           IF          (TWA-C-DBS-NO-REC-FOUND)
           THEN
             MOVE      2                   TO      W-SWITCH
           ELSE
             IF        (LNR-C-NORMAL)
             THEN
-4-            MOVE    'Z601'              TO      MSG-P-OUT-CODE       
               PERFORM 9146-LN-ERR-MSG-OUT-RTN.
       49092-EXIT.
           EXIT.
------*---------------沖正：主檔資料還原------------------------------
       4910-UPDATE-RVS.
           MOVE        LNL-STATUS          TO     W-LNL-STATUS.
970929     MOVE        LNL-LN-BAL          TO     WK-WORK-AMT.          
           MOVE        REV-L-RECV-INT-DATE TO     LNL-L-RECV-INT-DATE.  
           MOVE        REV-RECV-INT-DATE   TO     LNL-RECV-INT-DATE.
           MOVE        REV-INT-ADJ-DATE    TO     LNL-NEXT-INT-ADJ-DATE.
           MOVE        REV-INT-RATE        TO     LNL-INT-RATE.
021210*---------設定：〔ＬＮＭ帳戶餘額異動數〕                        
"          IF        ( LNL-LN-BAL  NOT <  ZEROS )                       
"                AND ( REV-LN-BAL  NOT <  ZEROS )                       
"                      MOVE    CTF-RECV-AMT      TO   WK-LNM-TXN-AMT    
"          ELSE IF   ( LNL-LN-BAL      <  ZEROS )                       
"                AND ( REV-LN-BAL  NOT >  ZEROS )                       
"                      MOVE    ZEROS             TO   WK-LNM-TXN-AMT    
"          ELSE IF   ( LNL-LN-BAL      <  ZEROS )                       
"                AND ( REV-LN-BAL      >  ZEROS )                       
"                      MOVE    REV-LN-BAL        TO   WK-LNM-TXN-AMT    
"     *---------------※以下狀況不可能發生                            
"          ELSE                                                         
"                      MOVE    ZEROS             TO   WK-LNM-TXN-AMT.   
"     *    ENDIF                                                        
           MOVE        REV-LN-BAL          TO     LNL-LN-BAL.
A01114     MOVE        REV-INSUR-AMT       TO     LNL-INSUR-AMT.
           MOVE        REV-RTRN-INTVL      TO     LNP-RTRN-INTVL.
980204     MOVE        REV-LAST-RTRN-DATE  TO     LNL-LAST-RTRN-DATE-R. 
           MOVE        REV-EACH-RTRN-AMT   TO     LNL-EACH-RTRN-AMT.
           MOVE        REV-NEXT-RTRN-DATE  TO     LNL-NEXT-RTRN-DATE.
           MOVE        REV-LNL-STATUS      TO     LNL-STATUS.
920527     MOVE        REV-ADJ-ALNRATE     TO     LNP-INT-ADJ-INTVL.    
950526     MOVE        REV-LNP-INT-CODE    TO     LNP-INT-CODE.         
950526     MOVE        REV-LNL-INT-CODE    TO     LNL-INT-CODE.         
950526     MOVE        REV-LNP-IN-DE-RATE  TO     LNP-IN-DE-INT-CODE.   
950526     MOVE        REV-INCR-ALNRATE    TO   LNL-INT-INCR-DECR-RATE. 
991203     MOVE        REV-EIR             TO   LNL-EIR.                
991203     MOVE        REV-CARRYING-AMT    TO   LNL-CARRYING-AMT.       
991203     MOVE        REV-NOT-YET-RTRN-FREE TO LNL-NOT-YET-RTRN-FREE.  
991203     MOVE        REV-RECVABL-ADJ-AMT TO   LNL-RECVABL-ADJ-AMT.    
830202     IF          NOT   (LNP-C-ACCUM)
           THEN
             IF   NOT  LNP-C-ENT-HGHER-SCH-LN
890209             OR  ((LNM-C-ACC-LN-NORMAL)                           
"                 AND  (LNP-C-ENT-HGHER-SCH-LN))                        
             THEN
               MOVE    REV-LAST-TXN-DATE   TO     LNM-LAST-TXN-DATE.
           IF          LNP-C-ENT-HGHER-SCH-LN
890209           AND   LNM-C-ACC-LN-SCHOOL                              
                 AND   W-LNL-C-RTRN
           THEN
             SUBTRACT  1                   FROM   LNM-RTRN-NO.
           MOVE        REV-BAL-SW          TO     LNM-BAL-SW.
           COMPUTE     LNL-RECV-RTRN-DIFF = LNL-RECV-RTRN-DIFF - W-DIFF.
      *---------------循環案件：加回未用額度                          
           IF          ((LNP-C-CYL-PAY)
                   AND  (NOT W-CTF-C-TR-OVER))
           THEN
130910*---------------排除：非本金轉催收或呆帳案件                    
"            IF      ( LNL-ACC-CODE  =  '47' OR '48' OR '49' OR '57'    
"                                    OR '58' )                          
"                AND ( LNL-BC-COLM   =  '1' OR '2' )                    
"            THEN                                                       
"                      NEXT    SENTENCE                                 
"            ELSE                                                       
                       COMPUTE W-LNP-APRV-UNUSED-BAL                    
                                  =  W-LNP-APRV-UNUSED-BAL - W-CTF-AMT  
             END-IF.                                                    
           COMPUTE     LNL-LNI-NO = LNL-LNI-NO - W-LNI-NO.
           COMPUTE     LNL-LNR-NO = LNL-LNR-NO - W-LNR-NO.
           IF          (W-LNL-C-RTRN
                    OR  W-LNL-C-TR
                    OR  W-LNL-C-TR-OVER)
970929         OR                                                       
"                   (( LNL-ACC-CODE      =  '48' OR '49' )              
"                AND ( WK-WORK-AMT   NOT >  ZEROS )                     
"                AND ( LNL-LN-BAL        >  ZEROS ))                    
           THEN
             ADD       1                   TO      LNP-LN-NO.
           IF          REV-OVERDUE-SW      =      1
           THEN
             COMPUTE   LNL-OVERDUE-RECV-NO =     LNL-OVERDUE-RECV-NO
                                                 - 1.
           PERFORM     49024-WRITE-BACK    THRU    49024-EXIT.
       4910-EXIT.
           EXIT.
------*---------------優惠利率：設定最後迄日--------------------------
891019 499600-SEAR-END-DATE-RTN.                                        
           ADD         1                   TO      END-RATE-INDEX.      
930518     IF        ( END-RATE-INDEX  >  4 )                           
"          THEN                                                         
"                      GO  TO  499600-EXIT.                             
           MOVE        ALNRAT-E-DATE(END-RATE-INDEX)                    
                                           TO      WKS-END-TEMP-DATE.   
           IF          WKS-END-TEMP-DATE   NOT     =  ZEROS             
           THEN                                                         
             MOVE      WKS-END-TEMP-DATE   TO      WKS-END-RATE-DATE.   
       499600-EXIT.                                                     
           EXIT.                                                        
------*---------------優惠利率：找出繳息日後隸屬的調整段--------------
891019 499601-SEAR-PRE-RATE-RTN.                                        
           ADD         1                   TO      CHG-RATE-INDEX.      
930518     IF        ( CHG-RATE-INDEX  >  4 )                           
"          THEN                                                         
"                      GO  TO  499601-EXIT.                             
           MOVE        ALNRAT-E-DATE(CHG-RATE-INDEX)                    
                                           TO      WKS-PRE-E-DATE.      
      *---------------第一段內                                        
900706     IF          CTF-INT-END-DATE-N-R <        ALNRAT-FE-DATE     
           THEN                                                         
             MOVE      'N'                 TO      WKS-CHG-SW           
             GO        TO      499601-EXIT.                             
921121     IF          CTF-INT-END-DATE-N-R   <      WKS-PRE-E-DATE     
           THEN                                                         
000628     MOVE        ALNRAT-ADJ-CODE(CHG-RATE-INDEX) TO SAVE-ADJ-CODE 
             MOVE      'Y'                 TO      WKS-CHG-SW           
             GO        TO      499601-EXIT.                             
       499601-EXIT.                                                     
           EXIT.                                                        
------*---------------優惠利率：尋找繳息迄日適用利率------------------
891019 499602-SEAR-INT-RATE-RTN.                                        
990609     IF        ( LNP-LN-CODE =  39 OR 69 )                        
"                AND ( LNP-CAPTL-PURPOSE  =  73 )                       
"                AND ( LNP-RECV-INT-TYPE  =  0)                         
"                AND ( LNP-C-AGREE-R )                                  
"                AND ( LNP-C-ADJ-FIX )                                  
"          THEN                                                         
"                      GO  TO  499602-EXIT.                             
970229*---------------設定：指數型房貸第一段採約定利率註記            
"          MOVE        'N'                  TO   WK-HOUS23-MARK.        
"          IF        ( LNP-INT-CODE       =  '92' )                     
"                AND ( LNP-CT-ISSUE-DATE  NUMERIC )                     
"                AND ( LNP-CT-ISSUE-DATE  >  ZEROS )                    
"          THEN                                                         
"                      SET     IDX26             TO   01                
"                      SEARCH  HOUS23-DATE-TABLE                        
"                      AT END  MOVE    'N'       TO   WK-HOUS23-MARK    
"                        WHEN  ( LNP-CT-ISSUE-DATE                      
"                                           = TBL-HOUS23-DATE(IDX26) )  
"                              MOVE    'Y'       TO   WK-HOUS23-MARK.   
"     *    ENDSEARCH                                                    
      *---------------「低利指數房貸」                                
970229     IF       (( LNP-C-ADJ-FIX          )                         
"                AND ( LNP-C-AGREE-R          )                         
"                AND ( WK-HOUS23-MARK  =  'Y' ))                        
951106*---------------「轉貸房貸搭配信用貸款專案」（存單開發日951110  
"             OR    (( LNP-C-ADJ-FIX )                                  
"                AND ( LNP-C-AGREE-R )                                  
"                AND ( LNP-CT-ISSUE-DATE   =  951110 ))                 
000311*-------------〔房貸壽險保費貸款／存單開發日1000301〕         
"             OR    (( LNP-C-ADJ-FIX )                                  
"                AND ( LNP-C-AGREE-R )                                  
"                AND ( LNP-LN-CODE        =  '00' )                     
"                AND ( LNP-CT-ISSUE-DATE  =  1000301 ))                 
030328*-------------〔特定授信案件轉換利率別〕                        
"             OR    (( LNM-ACC-NO  =  210166082956 )                    
030328           AND ( LNP-APRV-NO =  16295009801 )                     
030328           AND ( LNL-LN-SER-NO  =  0019 ))                        
"             OR    (( LNM-ACC-NO  =  210166082956 )                    
030328           AND ( LNP-APRV-NO =  16295009900 )                     
030328           AND ( LNL-LN-SER-NO  =  0017 ))                        
           THEN                                                         
                       MOVE   23           TO      LNP-INT-CODE         
                       MOVE   23           TO      LNL-INT-CODE-1.      
000628*-------------〔青年安心專案－混合固定／存單開發日1000501〕   
000628     IF       (( LNP-C-ADJ-FIX )                                  
"                AND ( LNP-C-AGREE-R )                                  
"                AND ( LNP-LN-CODE        =  '21' )                     
"                AND ( LNP-CT-ISSUE-DATE  =  1000501 ))                 
"          THEN IF ( SAVE-ADJ-CODE =  '1' )                             
"                      MOVE   21           TO      LNP-INT-CODE         
"                      MOVE   21           TO      LNL-INT-CODE-1       
"                ELSE                                                   
"                      MOVE   0            TO      SEA-P-INT-RATE-1     
"                      GO     TO           499602-EXIT.                 
           PERFORM     9171-LN-INT-AREA-CLEAR.                          
           MOVE        INT-M-EXEC-S        TO      INT-F-SERV-TYPE-SRHI.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.   
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.       
           MOVE        SEA-M-FUN-1         TO      SEA-P-FUN-CODE.      
           MOVE        6                   TO      SEA-P-INT-BUSNS.     
           MOVE        LNL-INT-CODE-1      TO      SEA-P-INT-RATE-CODE. 
900706     MOVE        CTF-INT-END-YY-N    TO      SEA-P-SPECIAL-YY-1.  
900706     MOVE        CTF-INT-END-MM-N    TO      SEA-P-SPECIAL-MM-1.  
900706     MOVE        CTF-INT-END-DD-N    TO      SEA-P-SPECIAL-DD-1.  
941006*---------------特殊境遇婦女採固定以初貸日利率為基準            
"          IF        ( LNP-CAPTL-PURPOSE  =  '72' )                     
"                AND ( LNP-INT-ADJ-INTVL  =  '00' )                     
"          THEN                                                         
"                      MOVE    LNL-1ST-LN-DATE-R TO   SEA-P-BEG-DATE-R. 
980506     EXEC        CICS                LINK    PROGRAM ('NACINT7')  
                       COMMAREA (INT-GROUP)        LENGTH (INT-LENGTH)  
                       END-EXEC.                                        
           PERFORM     9163-LN-RATE-SEA-ERR-HDL-RTN.                    
       499602-EXIT.                                                     
           EXIT.                                                        
------*---------------分段優惠利率：更新主檔--------------------------
891019 499603-COMP-INT-RATE-RTN.                                        
           MOVE        ALNRAT-IN-DE(CHG-RATE-INDEX)   TO  WKS-RATE2.    
990609     IF        ( LNP-LN-CODE =  39 OR 69 )                        
"          THEN                                                         
"            IF      ( LNP-CAPTL-PURPOSE  =  73 )                       
"                AND ( LNP-RECV-INT-TYPE  =  0)                         
"                AND ( LNP-C-AGREE-R )                                  
"                AND ( LNP-C-ADJ-FIX )                                  
"            THEN                                                       
"                MOVE     0                       TO  SEA-P-INT-RATE-1  
"                MOVE     SEA-P-INT-RATE-1        TO  WKS-RATE3         
"                COMPUTE  WKS-RATE4  =  WKS-RATE3  +  WKS-RATE2         
"                GO  TO  499603-SPEC-RTN.                               
           MOVE        SEA-P-INT-RATE-1           TO  WKS-RATE3.        
910731     COMPUTE     WKS-RATE4  =    WKS-RATE3   +  WKS-RATE2.        
920527******************************************************************
"     *※有設定利率調整方式＆與主檔一致＆加減利率＆與主檔一致         
"     *   ---視為調整過，不處理                                       
"     ******************************************************************
"          MOVE        ZEROS                TO   WK-ADJ-CODE.           
"          MOVE  ALNRAT-ADJ-CODE(CHG-RATE-INDEX) TO   WK-ADJ-CODE(2:1). 
"          IF        ( WK-ADJ-CODE(2:1)    NOT =  LOW-VALUE )           
"                AND ( WK-ADJ-CODE(2:1)    NOT =  SPACES )              
"                AND ( WK-ADJ-CODE             =  LNP-INT-ADJ-INTVL )   
"                AND ( LNL-INT-INCR-DECR-RATE  =  WKS-RATE2 )           
"          THEN                                                         
920527                 GO TO 499603-EXIT.                               
990609 499603-SPEC-RTN.                                                 
      ******************************************************************
920527*※新利率與主檔利率不同：更新「利率、加減利率、分期攤還額、     
      ******************************************************************
910731     IF          LNL-INT-RATE        =        WKS-RATE4           
           AND         LNL-INT-INCR-DECR-RATE =     WKS-RATE2           
           THEN                                                         
             NEXT      SENTENCE                                         
           ELSE                                                         
             COMPUTE   LNL-INT-RATE        =        WKS-RATE3           
                                           +        WKS-RATE2           
             MOVE      WKS-RATE2           TO       LNP-IN-DE-INT-CODE  
                                                LNL-INT-INCR-DECR-RATE  
920527       IF      ( LNP-RTRN-CODE  =  '2' OR '4' )                   
"              OR                                                       
"                   (( LNP-RTRN-CODE   =  '9'  )                        
"                AND ( LNP-RTRN-INTVL  =  '01' )                        
"                AND ( LNP-AFTER-TERM-RTRN-TYPE  =  '2'  ))             
"            THEN                                                       
031013                 PERFORM 4902A-ECHR-CAL-RTN     THRU  4902A-EXIT. 
"     *    ENDIF                                                        
       499603-EXIT.                                                     
           EXIT.                                                        
------*---------------分段優惠利率：更新主檔--------------------------
920527******************************************************************
"     *※新利率調整方式與主檔不同：更新「利率調整方式、下次利率調日」 
"     ******************************************************************
       499604-UPD-ADJ-DATA-RTN.                                         
"          MOVE        ZEROS                TO   WK-ADJ-CODE.           
"          MOVE  ALNRAT-ADJ-CODE(CHG-RATE-INDEX) TO    WK-ADJ-CODE(2:1).
"          IF        ( WK-ADJ-CODE(2:1) NOT = LOW-VALUE )               
"                AND ( WK-ADJ-CODE(2:1) NOT = SPACES )                  
"                AND ( WK-ADJ-CODE      NOT = LNP-INT-ADJ-INTVL )       
"          THEN                                                         
"                      MOVE  WK-ADJ-CODE    TO   LNP-INT-ADJ-INTVL      
"          ELSE                                                         
920527                 GO TO 499604-EXIT.                               
"     ******************************************************************
"     *※更新「下次利率調日」                                         
"     ******************************************************************
920527*---------------固定、機動：取到期日                            
"          IF        ( LNP-C-ADJ-FIX )                                  
"                 OR ( LNP-C-ADJ-FLOAT )                                
"          THEN                                                         
940701       IF      ( LNL-BC-COLM     =  'C' OR 'D' )                  
"                AND ( LNL-DUE-DATE-N  NUMERIC )                        
"                AND ( LNL-DUE-DATE-N  >  ZEROS )                       
"            THEN                                                       
"                      MOVE  LNL-DUE-YY-N   TO   LNL-INT-ADJ-YY         
"                      MOVE  LNL-DUE-MM-N   TO   LNL-INT-ADJ-MM         
"                      MOVE  LNL-DUE-DD-N   TO   LNL-INT-ADJ-DD         
"            ELSE                                                       
"                      MOVE  LNL-DUE-DATE-R TO   LNL-NEXT-INT-ADJ-DATE. 
920527*---------------一年調整一次、半年調整一次                      
"          IF        ( LNP-C-ADJ-1YR )                                  
"                 OR ( LNP-C-ADJ-HALF-YR )                              
980506            OR ( LNP-C-ADJ-1MM )                                  
"          THEN                                                         
"                      MOVE  LNL-1ST-LN-YY  TO   LNL-INT-ADJ-YY         
"                      MOVE  LNL-1ST-LN-MM  TO   LNL-INT-ADJ-MM         
"                      MOVE  LNL-1ST-LN-DD  TO   LNL-INT-ADJ-DD         
"                      PERFORM 9169-ADJ-INT-DATE-RTN                    
"                                 UNTIL   ( LNL-NEXT-INT-ADJ-DATE       
"                                             >  LNL-L-RECV-INT-DATE ). 
920527******************************************************************
"     *每年6/21調整、每年12/21調整、每年1/1調整、               
"     *每年6/21,12/21調整、每年3,6,9,12的21日調整               
"     ******************************************************************
"          IF        ( LNP-C-ADJ-EACH-6-21 )                            
"                 OR ( LNP-C-ADJ-EACH-12-21 )                           
"                 OR ( LNP-C-ADJ-EACH-1-01 )                            
"                 OR ( LNP-C-ADJ-6-21-12-21 )                           
"                 OR ( LNP-C-ADJ-3-6-9-12-21 )                          
"          THEN                                                         
"                      MOVE  LNL-L-RECV-INT-DATE                        
"                                           TO   LNL-NEXT-INT-ADJ-DATE  
"                      PERFORM 499605-ADJ-DATE-PROC-RTN                 
"                                                THRU  499605-EXIT.     
920527 499604-EXIT.                                                     
"             EXIT.                                                     
------*---------------固定日調整方式處理------------------------------
920527 499605-ADJ-DATE-PROC-RTN.                                        
"          MOVE    LNL-L-RECV-INT-DATE-R    TO   TXN-DATE-RESERVE-R.    
"          SUBTRACT    1                  FROM   TXN-YY                 
"                                       GIVING   LAST-YY.               
"          MOVE        TXN-YY     TO   FIRST-YY  SECOND-YY THIRD-YY     
"                                      FOURTH-YY FIFTH-YY.              
"          MOVE        1221       TO   LAST-M-D  SECOND-M-D.            
"          MOVE        0621                 TO   FIRST-M-D.             
"          MOVE        0101                 TO   THIRD-M-D.             
"          MOVE        0321                 TO   FOURTH-M-D.            
"          MOVE        0921                 TO   FIFTH-M-D.             
920527*---------------每年3/21,6/21,9/21,12/21調整                  
"          IF        ( LNP-C-ADJ-3-6-9-12-21 )                          
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  NOT <  SECOND-TERM-RANGE )     
"            THEN                                                       
"                      COMPUTE FIRST-YY     =    FIRST-YY   +   1       
"                      COMPUTE SECOND-YY    =    SECOND-YY  +   1       
"                      COMPUTE FOURTH-YY    =    FOURTH-YY  +   1       
"                      COMPUTE FIFTH-YY     =    FIFTH-YY   +   1       
"                      PERFORM 499606-DETAIL-ADJ-RTN  THRU  499606-EXIT 
"            ELSE                                                       
"                      PERFORM 499606-DETAIL-ADJ-RTN  THRU  499606-EXIT.
920527*---------------每年1/1調整                                   
"          IF        ( LNP-C-ADJ-EACH-1-01 )                            
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  NOT <  THIRD-TERM-RANGE )      
"            THEN                                                       
"                      COMPUTE THIRD-YY  =  THIRD-YY  +   1             
"                      MOVE  THIRD-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"                      MOVE  THIRD-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R.    
920527*---------------每年6/21調整                                  
"          IF        ( LNP-C-ADJ-EACH-6-21 )                            
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  NOT <  FIRST-TERM-RANGE )      
"            THEN                                                       
"                      COMPUTE FIRST-YY  =  FIRST-YY  +   1             
"                      MOVE  FIRST-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"                      MOVE  FIRST-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R.    
920527*---------------每年12/21調整                                 
"          IF        ( LNP-C-ADJ-EACH-12-21 )                           
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  NOT <  SECOND-TERM-RANGE )     
"            THEN                                                       
"                      COMPUTE SECOND-YY  =  SECOND-YY  +   1           
"                      MOVE  SECOND-TERM-RANGE-R                        
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"                      MOVE  SECOND-TERM-RANGE-R                        
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R.    
920527*---------------每年6/21,12/21調整                            
"          IF        ( LNP-C-ADJ-6-21-12-21 )                           
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  <  FIRST-TERM-RANGE )          
"            THEN                                                       
"                      MOVE  FIRST-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"              IF    ( TXN-DATE-RESERVE  NOT <   FIRST-TERM-RANGE )     
"                AND ( TXN-DATE-RESERVE      <   SECOND-TERM-RANGE )    
"              THEN                                                     
"                      MOVE  SECOND-TERM-RANGE-R                        
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"              ELSE                                                     
"                IF  ( TXN-DATE-RESERVE  NOT <   SECOND-TERM-RANGE)     
"                THEN                                                   
"                      COMPUTE FIRST-YY  =  FIRST-YY  +  1              
"                      MOVE  FIRST-TERM-RANGE-R                         
920527                                 TO   LNL-NEXT-INT-ADJ-DATE-R.    
       499605-EXIT.                                                     
              EXIT.                                                     
------*---------------每年3/21,6/21,9/21,12/21調整------------------
920527 499606-DETAIL-ADJ-RTN.                                           
"          IF        ( TXN-DATE-RESERVE  <  FIRST-TERM-RANGE )          
"          THEN                                                         
"            IF      ( TXN-DATE-RESERVE  <  FOURTH-TERM-RANGE )         
"            THEN                                                       
"                      MOVE  FOURTH-TERM-RANGE-R                        
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"                      MOVE  FIRST-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"          ELSE                                                         
"            IF      ( TXN-DATE-RESERVE  <  FIFTH-TERM-RANGE )          
"            THEN                                                       
"                      MOVE  FIFTH-TERM-RANGE-R                         
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R     
"            ELSE                                                       
"                      MOVE  SECOND-TERM-RANGE-R                        
"                                      TO   LNL-NEXT-INT-ADJ-DATE-R.    
920527 499606-EXIT.                                                     
"             EXIT.                                                     
------*-----------------------------------------------------------------
       500-FILE-PROC-RTN.
890309*---------------活力貸－建購（中長期）                          
"          IF        ( W-CTF-RECV-TYPE  NOT < '1' )                     
"                AND ( W-CTF-RECV-TYPE  NOT > '7' )                     
"                AND ( LNL-ACC-CODE  =  '26' OR '36' OR '38' )          
"                AND ( LNP-LN-CODE   =  '79' )                          
"          THEN                                                         
"                      PERFORM 9181-READ-PBHOUS-RTN   THRU  9181-EXIT   
"                      MOVE    PB-H-PB-ACC-NO(1) TO   COMM-PB-ACC-NO    
"                      MOVE    CTF-RECV-AMT      TO   COMM-AMT          
"                      MOVE  PB-H-LN-APRV-NO(1)  TO   COMM-APRV-NO      
"                      MOVE  PB-H-LN-LNL-SER-NO(1) TO COMM-LN-SER-NO    
"            IF      ( TWA-C-TXN-NOT-REVERSE )                          
"            THEN                                                       
"                      MOVE    0                 TO   COMM-FUN-CODE     
"                      PERFORM 9182-LINK-PB-RTN       THRU  9182-EXIT   
"                      PERFORM 9184-UPD-TOT-FIL-RTN   THRU  9184-EXIT   
"            ELSE                                                       
"                      MOVE    1                 TO   COMM-FUN-CODE     
"                      PERFORM 9182-LINK-PB-RTN       THRU  9182-EXIT   
"                      PERFORM 9185-UPD-TOT-FIL-RTN   THRU  9185-EXIT.  
"     *    ENDIF                                                        
890309*---------------活力貸－修繕（中長期）                          
"          IF        ( W-CTF-RECV-TYPE  NOT < '1' )                     
"                AND ( W-CTF-RECV-TYPE  NOT > '7' )                     
"                AND ( LNL-ACC-CODE  =  '26' OR '36' OR '38' )          
"                AND ( LNP-LN-CODE   =  '80' )                          
"          THEN                                                         
"            IF      ( TWA-C-TXN-NOT-REVERSE )                          
"            THEN                                                       
"                      PERFORM 9181-READ-PBHOUS-RTN   THRU  9181-EXIT   
"                      MOVE    PB-H-PB-ACC-NO(2) TO   COMM-PB-ACC-NO    
"                      MOVE    CTF-RECV-AMT      TO   COMM-AMT          
"                      MOVE PB-H-LN-APRV-NO(2)   TO   COMM-APRV-NO      
"                      MOVE PB-H-LN-LNL-SER-NO(2) TO  COMM-LN-SER-NO    
"                      MOVE    0                 TO   COMM-FUN-CODE     
"                      PERFORM 9182-LINK-PB-RTN       THRU  9182-EXIT   
"                      PERFORM 9184-UPD-TOT-FIL-RTN   THRU  9184-EXIT   
"            ELSE                                                       
"                      MOVE    1                 TO   COMM-FUN-CODE     
"                      PERFORM 9182-LINK-PB-RTN       THRU  9182-EXIT   
"                      PERFORM 9185-UPD-TOT-FIL-RTN   THRU  9185-EXIT.  
"     *    ENDIF                                                        
900105*---------------共用額度檔（正常交易）                          
"          IF         ( LNP-CNTR-NO   NOT   =   ZEROS)                  
"               AND   ( TWA-C-TXN-NOT-REVERSE)                          
"               AND   ( CTF-RECV-AMT  NOT  =  ZEROS)                    
980518          AND   ( LNL-ACC-CODE  NOT  =  '47' )                    
"               AND   ( LNL-ACC-CODE  NOT  =  '48' )                    
"               AND   ( LNL-ACC-CODE  NOT  =  '49' )                    
900105     THEN                                                         
"                      PERFORM 500S-UPD-CNTR-FILE-RTN THRU 500S-EXIT    
"                      MOVE    ZEROS                   TO   I           
"                      PERFORM 500S1-REW-RTN          THRU 500S1-EXIT.  
"     *    ENDIF                                                        
900105*---------------共用額度檔（沖正交易）                          
"          IF         ( LNP-CNTR-NO   NOT   =   ZEROS)                  
"               AND   ( TWA-C-TXN-REVERSE)                              
"               AND   ( CTF-RECV-AMT  NOT  =  ZEROS)                    
980518          AND   ( LNL-ACC-CODE  NOT  =  '47' )                    
"               AND   ( LNL-ACC-CODE  NOT  =  '48' )                    
"               AND   ( LNL-ACC-CODE  NOT  =  '49' )                    
"          THEN                                                         
"                      PERFORM 501S-UPD-CNTR-FILE-RTN THRU 501S-EXIT    
"                      MOVE    ZEROS                   TO   I           
030123                 PERFORM 501S1-REW-RTN          THRU 501S1-EXIT.  
      *---------------失業勞工展延利息檔案更新                        
901109     PERFORM     561-LBI-UPDATE-RTN  THRU    561-EXIT.            
901109     MOVE        WK-LABOR-DELAY-INT   TO   L-M-LABOR-INT.         
010130*----------/ 維護聯徵當日貸放清償報送檔　/--------------------  
010130     IF  ( CTF-ACC-BUSINS-CODE NOT = 69 )                         
010130     AND ( CTF-RECV-TYPE       = '1' OR '6' )                     
010130     AND ( CTF-CAPTL-PURPOSE NOT = 50  )                          
010130     AND ( CTF-CAPTL-PURPOSE NOT = 70  )                          
010130     AND ( CTF-CAPTL-PURPOSE NOT = 71  )                          
031219     OR  (( CTF-RECV-TYPE       = '2' )                           
031219     AND  ( LNL-ACC-CODE  =  '48' OR '49' )                       
031219     AND  ( LNL-LN-BAL NOT > ZEROS  ))                            
           THEN                                                         
010130       PERFORM     51190-PROC-RLTM-FILE-RTN   THRU   51190-EXIT.  
001019*---------------處理：ＡＧＤＩ協議利差檔                        
"          MOVE        ZEROS                TO   WAGDI-DIFF-BAL.        
"          PERFORM     562-AGDI-PROC-RTN         THRU    562-EXIT.      
      *----------------                                                 
981125     IF        ( F24-MARK = 'Y' OR 'C' )                          
           THEN                                                         
                       PERFORM 571-F24-UPDATE-RTN  THRU    571-EXIT.    
931216*---------------連動ＰＢ扣帳                                    
"          IF        ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR )          
"                AND ( CTF-TR-ACC-NO-1  NOT =  ZEROS )                  
           THEN                                                         
931216                 PERFORM 750-UP-PB-RTN     THRU  750-EXIT.        
      *    ENDIF                                                        
981223*---------------傳真交易檔處理                                  
"          IF          CTF-BUSINS-CODE   NOT   =   ZEROS                
"          THEN                                                         
"              PERFORM   500A-FAX-RTN THRU  500A-EXIT.                  
000218*------------/現金交易≧伍拾萬－寫入／刪除CFMJIBD檔/---------*
"          IF      (( TWA-TXN-ID-CODE-1 = 671 OR 672)                   
"              AND   ( CTF-W-LOG-AMT NOT < 500000 ) )                   
"               OR (( TWA-TXN-ID-CODE-1 = 674 OR 675 )                  
"              AND   ( CTF-RECV-CASH-AMT NOT < 500000 ) )               
"          THEN                                                         
"                    PERFORM 550-PROC-CFMJIBD    THRU  550-EXIT.        
"     *    ENDIF                                                        
      *---------------認證、科子目帳務處理、寫LOG                     
           PERFORM     600-OUTPUT-TXN-INFO-RTN     THRU    600-EXIT.
0606XX*----５８呆帳－國宅轉呆帳
      *----４８呆帳BIF４９就學貸款信保理賠BIF其他一般放款         
961226     EVALUATE    LNL-ACC-CODE                                     
               WHEN    '48'                                             
"                      PERFORM 700A-LNBAD-BIF-PREPARE-RTN THRU 700A-EXIT
0606XX         WHEN    '58'                                             
"                      PERFORM 700G-BAD69-BIF-PREPARE-RTN THRU 700G-EXIT
               WHEN    '49'                                             
"                      PERFORM 700C-SCH-TRUST-BIF-RTN THRU 700C-EXIT    
               WHEN    OTHER                                            
"                      PERFORM 700-BIF-PREPARE-RTN  THRU  700-EXIT.     
810821     MOVE        TWA-TELLER-CODE     TO      L-M-TELLER-CODE.
960521*---------------連動６４８１交易                                
"          IF          ( LNL-ACC-CODE          = 47 OR 57 )             
971222            AND  ( CTF-CAPTL-PURPOSE     = 50 )                   
"                 AND    TWA-C-TXN-NOT-REVERSE                          
"          THEN                                                         
"            MOVE  CTF-ACC-NO         TO   LNTR-ACC-NO                  
"            MOVE  CTF-LN-ID-NO       TO   LNTR-LN-ID-NO                
"            MOVE  48                 TO   LNTR-ACC-CODE                
"            MOVE  CTF-RECV-AMT       TO   LNTR-AMT                     
"            MOVE  LNL-DUE-DATE       TO   LNTR-DUE-DATE                
"            MOVE   '0000'            TO   LNTR-RETURN-CODE             
"            MOVE   ZEROS             TO   LNTR-BEF-RTRN-CODE           
"            MOVE   0                 TO   LNTR-BEF-RECV-INT-TYPE       
"            MOVE   ZEROS             TO   LNTR-AFT-ACC-DEP-CODE        
"            MOVE   ZEROS             TO   LNTR-AFT-CTF-LN-ID-NO        
"            MOVE   ZEROS             TO   LNTR-AFT-LN-ID-NO            
"            MOVE   0                 TO   LNTR-AFT-APRV-DATE           
"            MOVE   ZEROS             TO   LNTR-AFT-APRV-AMT            
980720       MOVE   LNL-RELIEF-TYPE   TO   LNTR-RELIEF-TYPE             
"            PERFORM  501-SEARCH-ACC-RTN THRU  501-EXIT.                
"          IF          ( LNL-ACC-CODE          = 47 OR 57 )             
971222            AND  ( CTF-CAPTL-PURPOSE     = 50 )                   
"                 AND  ( TWA-C-TXN-REVERSE )                            
"          THEN                                                         
"            MOVE  CTF-ACC-NO         TO   LNTR-ACC-NO                  
"            MOVE  CTF-LN-ID-NO       TO   LNTR-LN-ID-NO                
"            MOVE  48                 TO   LNTR-ACC-CODE                
"            MOVE  CTF-RECV-AMT       TO   LNTR-AMT                     
"            MOVE  LNL-DUE-DATE       TO   LNTR-DUE-DATE                
"            MOVE   '0000'            TO   LNTR-RETURN-CODE             
"            MOVE   ZEROS             TO   LNTR-BEF-RTRN-CODE           
"            MOVE   ZEROS             TO   LNTR-BEF-RECV-INT-TYPE       
"            MOVE   ZEROS             TO   LNTR-AFT-ACC-DEP-CODE        
"            MOVE   ZEROS             TO   LNTR-AFT-CTF-LN-ID-NO        
"            MOVE   ZEROS             TO   LNTR-AFT-LN-ID-NO            
"            MOVE   ZEROS             TO   LNTR-AFT-APRV-YY             
"            MOVE   ZEROS             TO   LNTR-AFT-APRV-MM             
"            MOVE   ZEROS             TO   LNTR-AFT-APRV-DD             
"            MOVE   ZEROS             TO   LNTR-AFT-APRV-AMT            
980720       MOVE   ZEROS             TO   LNTR-RELIEF-TYPE             
"            PERFORM  501-SEARCH-ACC-RTN THRU  501-EXIT.                
"          IF          ( LNL-ACC-CODE          = 47 OR 57 )             
971222            AND  ( CTF-CAPTL-PURPOSE     = 50 )                   
"          THEN                                                         
"            EXEC CICS   LINK PROGRAM('LNL6481')                        
"                             COMMAREA(LNTR-DATA)                       
"                             LENGTH(172)         END-EXEC              
"            IF          LNTR-RETURN-CODE  NOT =   '0000'               
"            THEN                                                       
"              MOVE      LNTR-RETURN-CODE  TO   MSG-P-OUT-CODE          
"              PERFORM   9146-LN-ERR-MSG-OUT-RTN.                       
"     *    ENDIF                                                        
"          IF          ( LNL-ACC-CODE          = 47 OR 57 )             
971222            AND  ( CTF-CAPTL-PURPOSE     = 50 )                   
"                 AND  ( TWA-C-TXN-NOT-REVERSE )                        
"                 AND  ( CTF-ACC-BUSINS-CODE NOT = 69 )                 
"          THEN                                                         
"            MOVE      LNTR-BEF-RTRN-CODE      TO  BEF-RTRN-CODE        
"            MOVE      LNTR-BEF-RECV-INT-TYPE  TO  BEF-RECV-INT-TYPE    
130910****------------此欄為ＬＯＧ帳號的部別欄，不可以異動            
130910*******MOVE      LNTR-AFT-ACC-DEP-CODE   TO  AFT-ACC-DEP-CODE     
971222       MOVE      LNTR-AFT-CTF-LN-ID-NO   TO  AFT-CTF-LN-ID-NO     
"            MOVE      LNTR-AFT-LN-ID-NO       TO  AFT-LN-ID-NO         
"            MOVE      LNTR-AFT-APRV-DATE      TO  AFT-FINE-BGN-DATE    
"            MOVE      LNTR-AFT-APRV-DATE      TO  AFT-APRV-DATE        
"            MOVE      LNTR-AFT-APRV-AMT       TO  AFT-FOR-INT-AMT      
"            MOVE      LNTR-AFT-APRV-AMT       TO  AFT-APRV-AMT.        
           PERFORM     9148-LN-TXN-LOG-RTN.
      *---------------國宅：處理市府融資帳（住福會）                  
           IF         CTF-ACC-BUSINS-CODE  = 69
                AND  LNL-C-LOAN-FOR-OTHER
                AND  (W-ACC-FIN-ACC-SER-NO  NOT =  0)
                AND  (CTF-RECV-AMT          NOT =  0)
                AND  (NOT W-CTF-C-TR-OVER)
0606XX          AND  ( LNL-ACC-CODE          NOT = '58')
           THEN
           ADD        1                 TO        TWA-ACC-TXN-SEQ-NO
           PERFORM    9125-UPDATE-CTZN-RTN        THRU    9125-EXIT.
931216*---------------認證存摺扣款傳票（網銀繳款不用）                
931216     IF        ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR )          
                 AND ( CTF-TR-ACC-NO-1  NOT =  ZEROS )                  
050614           AND ( TWA-BR-CODE      NOT =  '026' )                  
           THEN                                                         
931216                 PERFORM 660-PRT-PB-SLIP   THRU  660-EXIT.        
      *    ENDIF                                                        
031215*---------------提前清償警語提示                                
141124     IF        ( LNP-LMT-RTN-MARK >= '1' )                        
"                AND ( LNP-LMT-RTN-MARK <= '3' )                        
110824*****IF        ( LNP-LMT-RTN-MARK  =  '1' OR '2' )                
**************** AND ( LNL-ACC-CODE  =  '26' OR '38' OR '25' OR '37' )  
"                AND ( CTF-RECV-TYPE     =  '1'  )                      
040327           AND ( TWA-C-TXN-NOT-REVERSE )                          
031215     THEN                                                         
"                      PERFORM 640-PRECLS-WARN-RTN    THRU  640-EXIT    
"          END-IF.                                                      
001019*---------------印製『轉帳收入、轉帳支出』傳票                  
"          IF        ( WAGDI-DIFF-BAL   >  ZEROS )                      
050614           AND ( TWA-BR-CODE  NOT =  '026' )                      
"          THEN                                                         
"                      PERFORM 692-TR-SLIP-PROC-RTN   THRU  692-EXIT.   
       500-EXIT.                                                        
           EXIT.                                                        
------*-----------------------------------------------------------------
981223 500A-FAX-RTN.                                                    
"          IF     TWA-C-TXN-NOT-REVERSE                                 
"          THEN                                                         
"            NEXT  SENTENCE                                             
"          ELSE                                                         
"            PERFORM  502A-FAX-RTN          THRU   502A-EXIT            
"            GO   TO  500A-EXIT.                                        
"          EXEC CICS HANDLE CONDITION                                   
"                    NOTOPEN(910-NOT-OPEN)                              
"                    NOTFND(500A-EXIT-99)                               
"                    ERROR(910-OTHER-ERROR)                             
"          END-EXEC.                                                    
"          MOVE   TWA-TXN-DAT7      TO      FAX-DATE.                   
"          MOVE   TWA-BR-CODE       TO      FAX-BR.                     
"          MOVE   CTF-BUSINS-CODE   TO      FAX-NUMBER.                 
"          MOVE   ZEROS             TO      FAX-TIME.                   
"          MOVE   ZEROS             TO      FAX-TM-CODE.                
"          MOVE   ZEROS             TO      FAX-TXN-SEQ-NO.             
"          MOVE   0                 TO      WK-FAX-TXN-STATUS.          
"      500A-FAX-RTN-N.                                                  
"          EXEC     CICS  READ      DATASET('PBFAX')                    
"                                   INTO(FAX-REC)                       
"                                   LENGTH(FAX-LENGTH)                  
"                                   RIDFLD(FAX-KEY)                     
"                                   GTEQ                                
"          END-EXEC.                                                    
"          IF     TWA-TXN-DAT7          =     FAX-DATE                  
"            AND  TWA-BR-CODE           =     FAX-BR                    
"            AND  CTF-BUSINS-CODE       =     FAX-NUMBER                
"          THEN                                                         
"                 NEXT  SENTENCE                                        
"          ELSE                                                         
"             GO   TO   500A-EXIT-99.                                   
"          IF     FAX-TXN-DB-CR  =  1                                   
"          THEN                                                         
"           IF    FAX-TXN-STATUS =  1                                   
"           THEN                                                        
"            MOVE   1                     TO  WK-FAX-TXN-STATUS         
"           ELSE                                                        
"            MOVE   9                     TO  WK-FAX-TXN-STATUS.        
"     *    ENDIF                                                        
"          ADD       1                    TO   FAX-TXN-SER.             
"          GO   TO   500A-FAX-RTN-N.                                    
"      500A-EXIT-99.                                                    
"          IF       WK-FAX-TXN-STATUS  =  1                             
"          THEN                                                         
"            PERFORM  502A-FAX-RTN          THRU   502A-EXIT            
"          ELSE                                                         
"            MOVE     'D183'     TO   MSG-P-OUT-CODE                    
"            PERFORM  9146-LN-ERR-MSG-OUT-RTN.                          
"      500A-EXIT.                                                       
"           EXIT.                                                       
"      502A-FAX-RTN.                                                    
"          EXEC CICS   HANDLE  CONDITION                                
"                      NOTOPEN (910-NOT-OPEN)                           
"                      DUPREC  (910-DUPREC)                             
"                      ERROR   (910-OTHER-ERROR)                        
"          END-EXEC.                                                    
"          MOVE   TWA-TXN-DAT7       TO      FAX-DATE.                  
"          MOVE   TWA-BR-CODE        TO      FAX-BR.                    
"          MOVE   CTF-BUSINS-CODE    TO      FAX-NUMBER.                
"          MOVE   TWA-TXN-TIME       TO      FAX-TIME.                  
"          MOVE   TWA-TM-CODE        TO      FAX-TM-CODE.               
"          MOVE   TWA-ACC-TXN-SEQ-NO TO      FAX-TXN-SEQ-NO.            
"          MOVE     2                TO      FAX-TXN-DB-CR.             
"          MOVE    TWA-TXN-ID-CODE   TO      FAX-TXN-ID-CODE.           
"          MOVE    WK-FAX-ACC        TO      FAX-TR-OUT-ACC.            
"          MOVE    ZEROS             TO      FAX-TR-OUT-AMT.            
"          MOVE    016               TO      FAX-TR-IN-BANK.            
"          MOVE    ZEROS             TO      FAX-TR-IN-ACC-A.           
"          MOVE    CTF-ACC-NO        TO      FAX-TR-IN-ACC.             
"          COMPUTE WK-FAX-AMT = CTF-RECV-AMT + CTF-INT-AMT + CTF-FINE.  
"          MOVE    WK-FAX-AMT        TO      FAX-TR-IN-AMT.             
"          MOVE    TWA-TELLER-CODE   TO      FAX-TELLER.                
"          MOVE    CTF-SUP-CODE      TO      FAX-SUP.                   
"          MOVE    9                 TO      FAX-CONTRACT.              
"          IF      TWA-C-TXN-NOT-REVERSE                                
"          THEN                                                         
"                  MOVE    1         TO      FAX-TXN-STATUS             
"          ELSE                                                         
"                  MOVE    0         TO      FAX-CONTRACT               
"                  MOVE    9         TO      FAX-TXN-STATUS.            
"          IF          LNM-TAX-ID          =    ZERO OR SPACE           
"          THEN                                                         
"            MOVE      LNM-CUST-ID         TO   FAX-ID-CODE             
"          ELSE                                                         
"            MOVE      '00'                TO   FAX-TAX-00              
"            MOVE      LNM-TAX-ID          TO   FAX-TAX-CODE.           
"          MOVE    0                 TO      FAX-CF-SER.                
"          EXEC  CICS  WRITE  DATASET ('PBFAX')                         
"                             RIDFLD(FAX-KEY)                           
"                             FROM(FAX-REC)                             
"                             LENGTH(FAX-LENGTH)                        
"          END-EXEC.                                                    
"      502A-EXIT.                                                       
"           EXIT.                                                       
960216 501-SEARCH-ACC-RTN.                                              
020524*---------------設定：ＢＣ註記                                  
960827     IF          LNL-BC-COLM       =  '1'                         
"               OR     ( TWA-C-TXN-REVERSE )                            
"          THEN                                                         
"                      MOVE  LNL-BC-COLM  TO   LNTR-MARK.               
"     *    ENDIF                                                        
960827     IF          TWA-C-TXN-NOT-REVERSE                            
"          THEN                                                         
"            IF        LNL-BC-COLM  NOT  =  '1'                         
"            THEN                                                       
"                      MOVE     '0'       TO   LNTR-MARK.               
"     *    ENDIF                                                        
020524*---------------設定：種類代碼                                  
"     *---------------０２：現金卡                                    
"          IF       ( LNL-ACC-CODE  =  '23' OR '25' OR '37' )           
"               AND ( LNP-CT-TYPE   =  '76' OR '77' OR '59' )           
"                     MOVE     '02'              TO   LNTR-KIND         
020524*---------------０３：信用卡                                    
"          ELSE IF  ( LNP-LN-CODE  =  '78' )                            
"                     MOVE     '03'              TO   LNTR-KIND         
020524*---------------０４：就學貸款                                  
"          ELSE IF  ( LNP-LN-CODE  =  '55' )                            
"                     MOVE     '04'              TO   LNTR-KIND         
020524*---------------０５：應收承購帳款                              
"          ELSE IF  ( LNL-ORGL-ACC  =  '41' )                           
"                     MOVE     '05'              TO   LNTR-KIND         
020524*---------------０６：應收承兌票款                              
"          ELSE IF  ( LNL-ORGL-ACC  =  '42' )                           
"                     MOVE     '06'              TO   LNTR-KIND         
020524*---------------０１：一般放款                                  
"          ELSE IF  ( LNL-ORGL-ACC  NOT <  '20' )                       
"               AND ( LNL-ORGL-ACC  NOT >  '38' )                       
"                     MOVE     '01'              TO   LNTR-KIND         
020524*---------------０７：其他應收款                                
"          ELSE                                                         
"                     MOVE     '07'              TO   LNTR-KIND.        
"     *    ENDIF                                                        
"      501-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
900105 500S-UPD-CNTR-FILE-RTN.                                          
030123     EXEC CICS   HANDLE CONDITION                                 
"                      ENDFILE(500S-EXIT)                               
040317                 NOTOPEN(911-FILE-NOTOPEN)                        
"                      NOTFND (911-FILE-NOTFND)                         
"                      ERROR  (911-FILE-OTHER-ERROR)                    
"                      END-EXEC.                                        
"          IF          LNM-TAX-ID          =   SPACES                   
"                OR    LNM-TAX-ID          =   '00000000'               
"          THEN                                                         
"                      MOVE   LNM-CUST-ID  TO      SAVE-CUST-ID         
"          ELSE                                                         
"                      MOVE   '00'         TO      SAVE-CUST-ID1        
"                      MOVE   LNM-TAX-ID   TO      SAVE-CUST-ID2.       
"          MOVE        SAVE-CUST-ID        TO      LNCNTR-ID-NO.        
"          MOVE        LNM-ACC-NO          TO      LNCNTR-ACC-NO.       
"          MOVE        LNP-CNTR-NO         TO      LNCNTR-NO.           
"      500S-NEXT-RTN.                                                   
"          MOVE        ZEROS      TO     WK-CHK-AMT.                    
"          COMPUTE     I     =    I  +  1.                              
"          EXEC  CICS  READ                                             
"                      DATASET('ALNCNTR')                               
"                      INTO(LNCNTR)                                     
"                      LENGTH(TOT-APRV-LEN)                             
"                      RIDFLD(LNCNTR-KEY)                               
"          END-EXEC.                                                    
111003*---------------捨去角分再扣減                                  
111007     MOVE        CTF-RECV-AMT         TO   WK-TEMP-AMT.           
"          COMPUTE     LNCNTR-NT-BAL   =    LNCNTR-NT-BAL - WK-TEMP-AMT.
"          MOVE        LNCNTR-REL-NO       TO      SAVE-REL-NO.         
"          MOVE        LNCNTR-ID-NO        TO      WK-LNCNTR-ID-NO(I).  
"          MOVE        LNCNTR-ACC-NO       TO      WK-LNCNTR-ACC-NO(I). 
"          MOVE        LNCNTR-NO           TO      WK-LNCNTR-NO(I).     
"          MOVE        LNCNTR-NT-BAL       TO      WK-LNCNTR-NT-BAL(I). 
"          IF          LNCNTR-REL-NO      =   ZEROS                     
"          THEN                                                         
"                      NEXT  SENTENCE                                   
"          ELSE                                                         
"                      MOVE   SAVE-CUST-ID   TO   LNCNTR-ID-NO          
"                      MOVE   LNM-ACC-NO     TO   LNCNTR-ACC-NO         
"                      MOVE   LNCNTR-REL-NO  TO   LNCNTR-NO             
"                      GO     TO   500S-NEXT-RTN.                       
"      500S-EXIT.                                                       
"          EXIT.                                                        
"      500S1-REW-RTN.                                                   
"          COMPUTE     I        =             I   +   1.                
"          IF          I        >             4                         
"          THEN                                                         
"                      GO   TO  500S1-EXIT.                             
"     *    EXEC CICS   HANDLE CONDITION                                 
"          IF          WK-LNCNTR-ID-NO(I)     =       SPACES            
"               AND    WK-LNCNTR-ACC-NO(I)    =       ZEROS             
"               AND    WK-LNCNTR-NO(I)        =       ZEROS             
"               AND    WK-LNCNTR-NT-BAL(I)    =       ZEROS             
"          THEN                                                         
"                      GO   TO   500S1-REW-RTN.                         
"          MOVE        WK-LNCNTR-ID-NO(I)     TO      LNCNTR-ID-NO.     
"          MOVE        WK-LNCNTR-ACC-NO(I)    TO      LNCNTR-ACC-NO.    
"          MOVE        WK-LNCNTR-NO(I)        TO      LNCNTR-NO.        
"          EXEC  CICS  READ                                             
"                      DATASET('ALNCNTR')                               
"                      INTO(LNCNTR)                                     
"                      LENGTH(TOT-APRV-LEN)                             
"                      RIDFLD(LNCNTR-KEY)                               
"                      UPDATE                                           
"          END-EXEC.                                                    
"          MOVE        WK-LNCNTR-NT-BAL(I)   TO    LNCNTR-NT-BAL.       
"          EXEC        CICS             REWRITE    DATASET('ALNCNTR')   
"                                                  FROM(LNCNTR)         
"                                                  LENGTH(TOT-APRV-LEN) 
"          END-EXEC.                                                    
"          GO    TO    500S1-REW-RTN.                                   
"      500S1-EXIT.                                                      
"            EXIT.                                                      
------*-----------------------------------------------------------------
"      501S-UPD-CNTR-FILE-RTN.                                          
030123     EXEC CICS   HANDLE CONDITION                                 
"                      ENDFILE(501S-EXIT)                               
040317                 NOTOPEN(911-FILE-NOTOPEN)                        
"                      NOTFND (911-FILE-NOTFND)                         
"                      ERROR  (911-FILE-OTHER-ERROR)                    
"                      END-EXEC.                                        
"          IF          LNM-TAX-ID          =   SPACES                   
"                OR    LNM-TAX-ID          =   '00000000'               
"          THEN                                                         
"                      MOVE   LNM-CUST-ID  TO      SAVE-CUST-ID         
"          ELSE                                                         
"                      MOVE   '00'         TO      SAVE-CUST-ID1        
"                      MOVE   LNM-TAX-ID   TO      SAVE-CUST-ID2.       
"          MOVE        SAVE-CUST-ID        TO      LNCNTR-ID-NO.        
"          MOVE        LNM-ACC-NO          TO      LNCNTR-ACC-NO.       
"          MOVE        LNP-CNTR-NO         TO      LNCNTR-NO.           
"      501S-NEXT-RTN.                                                   
"          MOVE        ZEROS      TO     WK-CHK-AMT.                    
"          COMPUTE     I     =    I  +  1.                              
"          EXEC  CICS  READ                                             
"                      DATASET('ALNCNTR')                               
"                      INTO(LNCNTR)                                     
"                      LENGTH(TOT-APRV-LEN)                             
"                      RIDFLD(LNCNTR-KEY)                               
"          END-EXEC.                                                    
111003*---------------捨去角分再累加                                  
"          MOVE        CTF-RECV-AMT         TO   WK-TEMP-AMT.           
"          COMPUTE     LNCNTR-NT-BAL   =    LNCNTR-NT-BAL + WK-TEMP-AMT.
"          MOVE        LNCNTR-REL-NO       TO      SAVE-REL-NO.         
"          MOVE        LNCNTR-ID-NO        TO      WK-LNCNTR-ID-NO(I).  
"          MOVE        LNCNTR-ACC-NO       TO      WK-LNCNTR-ACC-NO(I). 
"          MOVE        LNCNTR-NO           TO      WK-LNCNTR-NO(I).     
"          MOVE        LNCNTR-NT-BAL       TO      WK-LNCNTR-NT-BAL(I). 
"          IF          LNCNTR-REL-NO      =   ZEROS                     
"          THEN                                                         
"                      NEXT  SENTENCE                                   
"          ELSE                                                         
"                      MOVE   SAVE-CUST-ID   TO   LNCNTR-ID-NO          
"                      MOVE   LNM-ACC-NO     TO   LNCNTR-ACC-NO         
"                      MOVE   LNCNTR-REL-NO  TO   LNCNTR-NO             
"                      GO     TO   501S-NEXT-RTN.                       
"      501S-EXIT.                                                       
"          EXIT.                                                        
"      501S1-REW-RTN.                                                   
"          COMPUTE     I        =             I       +   1.            
"          IF          I        >             4                         
"          THEN                                                         
"                      GO   TO  501S1-EXIT.                             
"          IF          WK-LNCNTR-ID-NO(I)     =       SPACES            
"               AND    WK-LNCNTR-ACC-NO(I)    =       ZEROS             
"               AND    WK-LNCNTR-NO(I)        =       ZEROS             
"               AND    WK-LNCNTR-NT-BAL(I)    =       ZEROS             
"          THEN                                                         
"                      GO   TO   501S1-REW-RTN.                         
"          MOVE        WK-LNCNTR-ID-NO(I)     TO      LNCNTR-ID-NO.     
"          MOVE        WK-LNCNTR-ACC-NO(I)    TO      LNCNTR-ACC-NO.    
"          MOVE        WK-LNCNTR-NO(I)        TO      LNCNTR-NO.        
"          EXEC  CICS  READ                                             
"                      DATASET('ALNCNTR')                               
"                      INTO(LNCNTR)                                     
"                      LENGTH(TOT-APRV-LEN)                             
"                      RIDFLD(LNCNTR-KEY)                               
"                      UPDATE                                           
"          END-EXEC.                                                    
"          MOVE        WK-LNCNTR-NT-BAL(I)   TO    LNCNTR-NT-BAL.       
"          EXEC        CICS             REWRITE    DATASET('ALNCNTR')   
"                                                  FROM(LNCNTR)         
"                                                  LENGTH(TOT-APRV-LEN) 
"          END-EXEC.                                                    
"          GO    TO     501S1-REW-RTN.                                  
"      501S1-EXIT.                                                      
"            EXIT.                                                      
010130*----------/ 維護聯徵當日貸放清償報送檔　/--------------------  
010130 51190-PROC-RLTM-FILE-RTN.                                        
"          MOVE        '016'                TO   WK-BANK-ID.            
090701     MOVE        LNM-AGENT-BR         TO   WK-FINCAL-BRANCH.      
"          PERFORM     51191-BR-CHK-CAL-RTN      THRU  51191-EXIT.      
"          EXEC      CICS    HANDLE    CONDITION                        
040317                                 NOTOPEN(911-FILE-NOTOPEN)        
"                                      NOTFND (911-FILE-NOTFND)         
"                                      DUPREC  (51190-REWRITE)          
"                                      ERROR   (51190-ERROR)            
"          END-EXEC.                                                    
"          IF        TWA-C-TXN-NOT-REVERSE   GO  TO  51190-WRITE.       
"          IF        TWA-C-TXN-REVERSE       GO  TO  51190-DELETE.      
"-----*------------/寫入ＬＮＪＲＬＴＭ檔/----------------------------*
"      51190-WRITE.                                                     
"          MOVE      SPACES                  TO  LNJRLTM.               
"          MOVE      WK-BANK-ID              TO  RLTM-BANK-ID.          
090701     MOVE      LNM-AGENT-BR            TO  RLTM-BR-CODE.          
"          MOVE      WK-BRANCH-CHK           TO  RLTM-BR-CHK.           
"          MOVE      TWA-TXN-DAT7            TO  RLTM-TXN-DATE.         
"          MOVE      CTF-ACC-NO              TO  RLTM-ACC-NO.           
"          MOVE      LNP-APRV-NO             TO  RLTM-APRV-NO.          
"          MOVE      LNL-LN-SER-NO           TO  RLTM-LN-SER-NO.        
"          MOVE      SPACES                  TO  RLTM-KEY-FILLER.       
"          IF        ( LNM-TAX-ID  =   SPACES    )                      
"                 OR ( LNM-TAX-ID  =   ZEROS     )                      
070330            OR ( LNM-ORGAN-TYPE  =  '2' )                         
"          THEN                                                         
"            MOVE     LNM-CUST-ID            TO  RLTM-CUST-ID           
"          ELSE                                                         
"            MOVE     LNM-TAX-ID             TO  RLTM-TAX-ID            
"            MOVE     SPACES                 TO  RLTM-TAX-TAIL.         
"          PERFORM     51192-JCIC-ACC-RTN      THRU  51192-EXIT.        
"          COMPUTE   WK-TEMP-AMT ROUNDED =  CTF-RECV-AMT / 1000.        
061030     IF        ( WK-TEMP-AMT   =  ZEROS )                         
"          THEN                                                         
061030                 MOVE    1            TO   WK-TEMP-AMT.           
"          MOVE      WK-TEMP-AMT             TO  RLTM-TXN-AMT.          
010202     MOVE      ZEROS                 TO  RLTM-DBR22-AMT.          
"          MOVE      'P'                     TO  RLTM-TXN-TYPE.         
"          COMPUTE   WK-TEMP-AMT ROUNDED =  LNP-APRV-AMT  / 1000.       
"          IF        ( WK-TEMP-AMT   =  ZEROS )                         
"                AND ( LNP-APRV-AMT  >  ZEROS )                         
"          THEN                                                         
"                      MOVE    1            TO   WK-TEMP-AMT.           
"          MOVE      WK-TEMP-AMT             TO  RLTM-APRV-AMT.         
"          EXEC      CICS    WRITE     DATASET ('LNJRLTM')              
"                                      FROM    (LNJRLTM)                
"                                      RIDFLD  (RLTM-KEY)               
"                                      LENGTH  (RLTM-LENGTH)            
"                    END-EXEC.                                          
"          GO   TO   51190-EXIT.                                        
------*------------/更新ＬＮＪＲＬＴＭ檔/----------------------------*
"      51190-REWRITE.                                                   
"          MOVE      SPACES                  TO  LNJRLTM.               
"          MOVE      WK-BANK-ID              TO  RLTM-BANK-ID.          
090701     MOVE      LNM-AGENT-BR            TO  RLTM-BR-CODE.          
"          MOVE      WK-BRANCH-CHK           TO  RLTM-BR-CHK.           
"          MOVE      TWA-TXN-DAT7            TO  RLTM-TXN-DATE.         
"          MOVE      CTF-ACC-NO              TO  RLTM-ACC-NO.           
"          MOVE      LNP-APRV-NO             TO  RLTM-APRV-NO.          
"          MOVE      LNL-LN-SER-NO           TO  RLTM-LN-SER-NO.        
"          MOVE      SPACES                  TO  RLTM-KEY-FILLER.       
"          EXEC      CICS    READ      DATASET ('LNJRLTM')              
"                                      INTO    (LNJRLTM)                
"                                      LENGTH  (RLTM-LENGTH)            
"                                      RIDFLD  (RLTM-KEY)               
"                                      UPDATE                           
"                      END-EXEC.                                        
"          IF        ( LNM-TAX-ID  =   SPACES    )                      
"                 OR ( LNM-TAX-ID  =   ZEROS     )                      
070330            OR ( LNM-ORGAN-TYPE  =  '2' )                         
"          THEN                                                         
"            MOVE     LNM-CUST-ID            TO  RLTM-CUST-ID           
"          ELSE                                                         
"            MOVE     LNM-TAX-ID             TO  RLTM-TAX-ID            
"            MOVE     SPACES                 TO  RLTM-TAX-TAIL.         
"          PERFORM     51192-JCIC-ACC-RTN      THRU  51192-EXIT.        
"          COMPUTE   WK-TEMP-AMT ROUNDED =  CTF-RECV-AMT / 1000.        
"          MOVE      WK-TEMP-AMT             TO  RLTM-TXN-AMT.          
"          IF   ( LNL-ACC-CODE = 21 OR 23 OR 25 OR 37 )                 
"          THEN                                                         
"            COMPUTE   WK-TEMP-AMT ROUNDED =  LNL-LN-BAL  / 1000        
"            MOVE      WK-TEMP-AMT           TO  RLTM-DBR22-AMT         
"          ELSE                                                         
"            MOVE      ZEROS                 TO  RLTM-DBR22-AMT.        
"          MOVE      'P'                     TO  RLTM-TXN-TYPE.         
"          COMPUTE   WK-TEMP-AMT ROUNDED =  LNP-APRV-AMT  / 1000.       
"          IF        ( WK-TEMP-AMT   =  ZEROS )                         
"                AND ( LNP-APRV-AMT  >  ZEROS )                         
"          THEN                                                         
"                      MOVE    1            TO   WK-TEMP-AMT.           
"          MOVE      WK-TEMP-AMT             TO  RLTM-APRV-AMT.         
"          EXEC      CICS    REWRITE   DATASET ('LNJRLTM')              
"                                      FROM    (LNJRLTM)                
"                                      LENGTH  (RLTM-LENGTH)            
"                      END-EXEC.                                        
"          GO   TO   51190-EXIT.                                        
"      51190-DELETE.                                                    
010305     EXEC      CICS    HANDLE    CONDITION                        
010305                                 NOTFND  (51190-EXIT)             
010305     END-EXEC.                                                    
"          MOVE      SPACES                  TO  LNJRLTM.               
"          MOVE      WK-BANK-ID              TO  RLTM-BANK-ID.          
090701     MOVE      LNM-AGENT-BR            TO  RLTM-BR-CODE.          
"          MOVE      WK-BRANCH-CHK           TO  RLTM-BR-CHK.           
"          MOVE      TWA-TXN-DAT7            TO  RLTM-TXN-DATE.         
"          MOVE      CTF-ACC-NO              TO  RLTM-ACC-NO.           
"          MOVE      LNP-APRV-NO             TO  RLTM-APRV-NO.          
"          MOVE      LNL-LN-SER-NO           TO  RLTM-LN-SER-NO.        
"          EXEC      CICS    DELETE    DATASET ('LNJRLTM')              
"                                      RIDFLD     (RLTM-KEY)            
"                    END-EXEC.                                          
"          GO   TO   51190-EXIT.                                        
040317*-------------其他錯誤                                          
"      51190-ERROR.                                                     
"          MOVE      'D608'                 TO   MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      51190-EXIT.                                                      
"            EXIT.                                                      
------*-----------------------------------------------------------------
"      51191-BR-CHK-CAL-RTN.                                            
"          MOVE        ZEROS                TO   WK-TEMP-SUM.           
"          COMPUTE     WK-TEMP-SUM      =   WS-RECV-BK-1 * 3            
"                                         + WS-RECV-BK-2 * 7            
"                                         + WS-RECV-BK-3 * 9            
"                                         + WS-RECV-BK-4 * 3            
"                                         + WS-RECV-BK-5 * 7            
"                                         + WS-RECV-BK-6 * 9.           
"          COMPUTE     WK-BRANCH-CHK   =    10  -  WK-TEMP-SUM-D3.      
"      51191-EXIT.                                                      
"          EXIT.                                                        
"-----*---------------設定：聯徵科目代碼------------------------------
"      51192-JCIC-ACC-RTN.                                              
"     *---------------存單質借                                        
"          IF       (( LNP-ACC-CODE  =  '24' )                          
"               AND  ( LNP-LN-CODE   =  '70' OR '71' ))                 
"             OR                                                        
"                   (( LNP-ACC-CODE  =  '24' OR '26' OR '38' )          
"               AND  ( LNP-LN-CODE   =  '03' OR '30'         ))         
"          THEN                                                         
"                      MOVE    'KS'         TO   RLTM-ACC-CODE          
"                      GO  TO  51192-EXIT.                              
"     *---------------助學貸款                                        
"          IF        ( LNP-ACC-CODE  =  '37' )                          
"               AND  ( LNP-LN-CODE   =  '55' )                          
"          THEN                                                         
"                      MOVE    'ZX'         TO   RLTM-ACC-CODE          
"                      GO  TO  51192-EXIT.                              
"     *---------------現金卡                                          
"          MOVE        LNP-ACC-CODE         TO   CDTN-ACC-CODE.         
"          MOVE        LNP-LN-CODE          TO   CDTN-LN-CODE.          
"          IF        ( CDTN-C-OVERDRAFT-LN )                            
"                AND ( LNP-ACC-CODE         =  '23' )                   
"                AND (  LNM-MAIN-BUSINS-CODE  NOT <  060000 )           
"                AND (  LNM-MAIN-BUSINS-CODE  NOT >  069999 )           
"          THEN                                                         
"                      MOVE    'YX'         TO   RLTM-ACC-CODE          
"                      GO  TO  51192-EXIT.                              
"     *---------------科目轉換（依聯徵規定）                          
"          MOVE        'O'                       TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '20' )  MOVE 'W'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '21'                                     
"                           OR '22' )  MOVE 'C'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '23'                                     
"                           OR '24' )  MOVE 'E'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '25'                                     
"                           OR '26' )  MOVE 'H'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '37'                                     
"                           OR '38' )  MOVE 'I'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '41' )  MOVE 'V'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '42' )  MOVE 'X'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '45' )  MOVE 'L'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '46' )  MOVE 'J'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '47' )  MOVE 'A'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '48' )  MOVE 'B'  TO   RLTM-ACC-CODE1.   
"          IF ( LNP-ACC-CODE = '49' )  MOVE 'B'  TO   RLTM-ACC-CODE1.   
"     *---------------商業本票保證                                    
"          IF        ( LNP-ACC-CODE  =  '45' )                          
"                AND ( LNP-LN-CODE   =  '89' )                          
"          THEN                                                         
"                      MOVE  'F'            TO   RLTM-ACC-CODE1.        
"     *---------------科目註記：放款科目－無擔保                      
"          MOVE        SPACES               TO   RLTM-ACC-CODE2.        
"          IF        ( LNL-ACC-CODE  =  '22' OR '24' OR '26' OR '38' )  
"              OR                                                       
"                   (( LNL-ACC-CODE  =  '47' )                          
"                AND ( LNL-ORGL-ACC  =  '22' OR '24' OR '26' OR '38' )) 
"          THEN                                                         
"                      MOVE    'S'          TO   RLTM-ACC-CODE2.        
"     *---------------科目註記：放款科目－擔保                        
"          IF        ( LNL-ACC-CODE  =  '21' OR '23' OR '25' OR '37' )  
"              OR                                                       
"                   (( LNL-ACC-CODE  =  '47' )                          
"                AND ( LNL-ORGL-ACC  =  '21' OR '23' OR '25' OR '37 ' ))
"          THEN                                                         
"                      MOVE    'X'          TO   RLTM-ACC-CODE2.        
"     ******************************************************************
"     *「應收承兌票款、應收保證款項、應收信用狀款項」                 
"     *發生墊款時，利用「分期攤還額」識別（營業單位以６２６變更），   
"     *科目註記：Ｍ－擔保、Ｎ－無擔                                   
"     ******************************************************************
"          IF        ( RLTM-ACC-CODE1  =  'F' OR 'L' OR 'X' )           
"                AND ( LNL-EACH-RTRN-AMT >  ZEROS )                     
"          THEN                                                         
"            IF     (  RLTM-ACC-CODE2  =  'S'  )                        
"            THEN                                                       
"                      MOVE    'M'          TO   RLTM-ACC-CODE2         
"            ELSE                                                       
"                      MOVE    'N'          TO   RLTM-ACC-CODE2.        
"     *---------------短放－墊付國內票款：票借短期貸款                
"          IF        ( LNP-ACC-CODE  =  '23' )                          
"                AND ( LNP-LN-CODE   =  '06' )                          
"          THEN                                                         
"                      MOVE  'D'            TO   RLTM-ACC-CODE1.        
040130     IF        ( RLTM-ACC-CODE2 =  ' ' )                          
"          THEN IF ( LNP-COLA-MARK  =  '1'  )                           
"                      MOVE    'S'          TO   RLTM-ACC-CODE2         
"               ELSE                                                    
"                      MOVE    'X'          TO   RLTM-ACC-CODE2.        
040130*    ENDIF                                                        
"      51192-EXIT.                                                      
010130     EXIT.                                                        
000218*------------/現金交易≧伍拾萬－寫入／刪除CFMJIBD檔/---------*
"      550-PROC-CFMJIBD.                                                
"          EXEC      CICS    HANDLE    CONDITION                        
"                                      NOTOPEN (550-NOTOPEN)            
"                                      DUPREC  (550-DUPREC)             
"                                      NOTFND  (550-NOTFND)             
"                                      ERROR   (550-ERROR)              
"                                      ENDFILE (550-ENDFILE)            
"          END-EXEC.                                                    
"          IF        TWA-C-TXN-NOT-REVERSE   GO  TO  550A-WRITE.        
"          IF        TWA-C-TXN-REVERSE       GO  TO  550B-DELETE.       
------*------------/寫入ＣＦＭＪＩＢＤ檔/正常交易------------------*
"      550A-WRITE.                                                      
"          MOVE      TWA-BR-CODE             TO  MJIBD-BR.              
"          MOVE      TWA-TXN-DAT7            TO  MJIBD-DAY.             
000519     MOVE      WK-LNM-CREAT-DATE       TO  MJIBD-OPEN-DAY.        
"          MOVE      CTF-ACC-NO              TO  MJIBD-ACNO.            
"          MOVE      TWA-TXN-TIME-HH         TO  MJIBD-HOUR.            
"          MOVE      TWA-TXN-TIME-MM         TO  MJIBD-MIN.             
"          MOVE      TWA-TXN-ID-CODE         TO  MJIBD-TXN-ID.          
"          MOVE      TWA-TM-ID-NO            TO  WK-TXN-SEQ-1.          
"          MOVE      TWA-ACC-TXN-SEQ-NO      TO  WK-TXN-SEQ-2.          
"          MOVE      WK-TXN-SEQ              TO  MJIBD-SEQ-NO.          
"          IF       ( TWA-TXN-ID-CODE-1 = 671 OR 672)                   
"          THEN                                                         
"            MOVE    CTF-W-LOG-AMT           TO  MJIBD-AMT.             
"          IF       ( TWA-TXN-ID-CODE-1 = 674 OR 675 )                  
"          THEN                                                         
"            MOVE    CTF-RECV-CASH-AMT       TO  MJIBD-AMT.             
"          MOVE      TWA-TELLER-CODE         TO  MJIBD-TELLER.          
B00309     MOVE      WS-TXN-CUST-NAME        TO  MJIBD-TXN-NAME.        
"          MOVE      ZEROS                   TO  MJIBD-STS              
"                                                MJIBD-920-DAY          
"                                                MJIBD-920-TIME         
"                                                MJIBD-FIL.             
041211     MOVE      WK-LNM-CFM-SER         TO   MJIBD-CF-SER.          
000519*---------------------寫檔                                      
"          EXEC      CICS    WRITE     DATASET ('CFMJIBD')              
"                                      FROM    (MJIBD-REC)              
"                                      RIDFLD  (MJIBD-KEY)              
"                                      LENGTH  (MJIBD-LEN)              
"                    END-EXEC.                                          
"          GO   TO   550-EXIT.                                          
------*------------/先讀取ＣＦＭＪＩＢＤ檔後再刪除/沖正交易--------*
"      550B-DELETE.                                                     
"          MOVE      TWA-BR-CODE             TO  MJIBD-BR.              
"          MOVE      TWA-TXN-DAT7            TO  MJIBD-DAY.             
"          MOVE      CTF-ACC-NO              TO  MJIBD-ACNO.            
"          MOVE      ZEROS                   TO  MJIBD-HOUR MJIBD-MIN.  
"      550B-READ-NEXT.                                                  
"          EXEC      CICS    READ      DATASET ('CFMJIBD')              
"                                      INTO    (MJIBD-REC)              
"                                      RIDFLD  (MJIBD-KEY)              
"                                      LENGTH  (MJIBD-LEN)              
"                                      GTEQ                             
"          END-EXEC.                                                    
"     *----------------------------------------------------------------*
"          IF      ( MJIBD-BR > TWA-BR-CODE )                           
"          THEN                                                         
"                    GO  TO  550-EXIT.                                  
"     *------------/核對是否為要沖正之資料/--------------------------*
"          IF      ( TWA-BR-CODE        NOT = MJIBD-BR     ) OR         
"                  ( TWA-TXN-DAT7       NOT = MJIBD-DAY    ) OR         
"                  ( CTF-ACC-NO         NOT = MJIBD-ACNO   ) OR         
"                  ( TWA-TXN-ID-CODE    NOT = MJIBD-TXN-ID ) OR         
"                  ( CTF-TXN-SER-NO     NOT = MJIBD-SEQ-NO )            
"          THEN                                                         
"                    ADD     1               TO  MJIBD-TIME             
"                    GO  TO  550B-READ-NEXT.                            
"          IF      (( TWA-TXN-ID-CODE-1 = 671 OR 672)                   
"              AND   ( CTF-W-LOG-AMT NOT = MJIBD-AMT ) )                
"               OR (( TWA-TXN-ID-CODE-1 = 674 OR 675 )                  
"              AND   ( CTF-RECV-CASH-AMT NOT = MJIBD-AMT ) )            
"          THEN                                                         
"                    ADD     1               TO  MJIBD-TIME             
"                    GO  TO  550B-READ-NEXT.                            
"     *------------/再次讀取以便刪除/--------------------------------*
"          EXEC      CICS    READ      DATASET ('CFMJIBD')              
"                                      INTO    (MJIBD-REC)              
"                                      RIDFLD  (MJIBD-KEY)              
"                                      LENGTH  (MJIBD-LEN)              
"                                      UPDATE                           
"                    END-EXEC.                                          
050524*-----------------------------------------------------------------
"     *申報完畢(920交易)資料不可沖正，需先做刪除後才可沖正        
"     *-----------------------------------------------------------------
"          IF        ( MJIBD-STS  =  9 )                                
"          THEN                                                         
"                      MOVE    'D190'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
"     *    ENDIF                                                        
000218*----------------------------------------------------------------*
"          EXEC      CICS    DELETE    DATASET ('CFMJIBD')              
"                    END-EXEC.                                          
"     *----------------------------------------------------------------*
"          GO   TO   550-EXIT.                                          
"      550-NOTOPEN.                                                     
"          MOVE     'D802'                   TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      550-DUPREC.                                                      
"          MOVE     'D179'                   TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      550-ENDFILE.                                                     
031013     GO   TO   550-EXIT.                                          
000519 550-NOTFND.                                                      
"          GO   TO   550-EXIT.                                          
"      550-ERROR.                                                       
"          IF      ( TWA-C-TXN-NOT-REVERSE )                            
"          THEN                                                         
"                    MOVE   'D184'           TO  MSG-P-OUT-CODE         
"          ELSE IF ( TWA-C-TXN-REVERSE )                                
"               THEN                                                    
"                    MOVE   'D804'           TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"     *    PERFORM   917-ERR-MSG-OUT-RTN.                               
"      550-EXIT.                                                        
"          EXIT.                                                        
------*---------------失業勞工展延利息處理----------------------------
901109 560-LBI-INT-HDL-RTN.                                             
901109     IF        ( LNL-LABOR-APPLY-CODE  NOT =  '1' )               
           THEN                                                         
                       GO TO 560-EXIT.                                  
           EXEC        CICS  HANDLE    CONDITION                        
930730                                 NOTOPEN(911-FILE-NOTOPEN)        
"                                      ERROR  (911-FILE-OTHER-ERROR)    
060421                                 NOTFND (560-EXIT)                
                       END-EXEC.                                        
           MOVE        CTF-ACC-NO      TO   LBI-ACC-NO.                 
           MOVE        CTF-LN-ID-NO    TO   LBI-LN-ID-NO.               
           EXEC        CICS READ DATASET('LNLBINT')                     
                                 INTO   (LNLBINT)                       
                                 LENGTH (LBI-LENGTH)                    
                                 RIDFLD (LBI-KEY)                       
                       END-EXEC.                                        
      *---------------沖正交易：利息抓REV                             
           IF        ( TWA-C-TXN-REVERSE )                              
           THEN                                                         
                       MOVE  REV-LABOR-INT  TO   WK-LABOR-DELAY-INT     
                       GO TO 560-EXIT.                                  
      *---------------正常交易：計算利息                              
           IF        ( LBI-UNPAY-INT  =  ZEROS )                        
           THEN                                                         
                       MOVE  ZEROS     TO   WK-LABOR-DELAY-INT          
                       GO TO 560-EXIT.                                  
      *---------------移出：不能做                                    
           IF        ( CTF-RECV-TYPE  =  6 )                            
           THEN                                                         
                       MOVE  'M60T'    TO   MSG-P-OUT-CODE              
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
      *---------------收回、展期、轉催：取剩餘未還利息                
           IF        ( CTF-RECV-TYPE  =  1 OR 3 OR 5 )                  
           THEN                                                         
                       MOVE  LBI-UNPAY-INT  TO   WK-LABOR-DELAY-INT     
                       GO TO 560-EXIT.                                  
      *---------------繳息迄日≧攤還迄日：取剩餘未還利息              
           IF        ( CTF-INT-END-DATE-N-R  NOT <  LBI-RTRN-DUE-DATE ) 
           THEN                                                         
                       MOVE  LBI-UNPAY-INT  TO   WK-LABOR-DELAY-INT     
                       GO TO 560-EXIT.                                  
      *---------------繳息迄日＜展延迄日（＝攤還起日）：填０          
           IF        ( CTF-INT-END-DATE-N-R  <  LBI-END-DATE )          
           THEN                                                         
                       MOVE  ZEROS     TO   WK-LABOR-DELAY-INT          
                       GO TO 560-EXIT.                                  
      *---------------重設上次繳息日：若與繳息日同月份會少算一期      
           IF        ( LNL-L-RECV-INT-DATE-R  >  LBI-END-DATE )         
           THEN                                                         
                       MOVE  LNL-RECV-INT-DATE   TO  WKS-L-RECV-INT-DATE
           ELSE                                                         
                       MOVE  LBI-END-DATE   TO   WKS-L-RECV-INT-DATE.   
           IF        ( WKS-L-RECV-INT-YY  =  LNL-RECV-INT-YY )          
                 AND ( WKS-L-RECV-INT-MM  =  LNL-RECV-INT-MM )          
           THEN                                                         
                   COMPUTE WKS-L-RECV-INT-MM = WKS-L-RECV-INT-MM - 1    
               IF    ( WKS-L-RECV-INT-MM  =  ZEROS )                    
               THEN                                                     
                       MOVE  12        TO   WKS-L-RECV-INT-MM           
                   COMPUTE WKS-L-RECV-INT-YY = WKS-L-RECV-INT-YY - 1.   
      *---------------計算上次繳息日的月底日                          
040317     MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
100406     MOVE        01                   TO   WK-DAT-BEG-DATE(6:2).  
"          PERFORM     808-DAT-08-CAL-RTN        THRU  808-EXIT.        
"          MOVE        DAT-P-CORR-DATE      TO   WKS-END-DATE.          
      *---------------設定上次繳息日的相對日                          
           IF        ( WKS-L-RECV-INT-DD  NOT =   LNL-RECV-INT-DAY )    
           THEN                                                         
                       MOVE  LNL-RECV-INT-DAY                           
                                       TO   WKS-L-RECV-INT-DD.          
           IF        ( WKS-L-RECV-INT-DATE  >  WKS-END-DATE )           
           THEN                                                         
                       MOVE  WKS-END-DD     TO   WKS-L-RECV-INT-DD.     
      *---------------計算本次攤還期數&應攤還利息                   
040317     MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
"          MOVE        CTF-INT-END-DATE-N-R TO   WK-DAT-END-DATE        
"          MOVE        LNL-RECV-INT-DAY     TO   WK-DAT-DAYS.           
"          MOVE        01                   TO   WK-DAT-TERM-UNIT.      
"          PERFORM     815-DAT-15-CAL-RTN        THRU  815-EXIT.        
           COMPUTE     WK-LABOR-DELAY-INT  =     LBI-EACH-RTRN-INT      
                                              *  DAT-P-TERM-NO.         
           IF        ( WK-LABOR-DELAY-INT  >  LBI-UNPAY-INT  )          
           THEN                                                         
                       MOVE  LBI-UNPAY-INT  TO   WK-LABOR-DELAY-INT.    
901109 560-EXIT.                                                        
901109     EXIT.                                                        
------*---------------失業勞工展延利息檔案更新------------------------
901109 561-LBI-UPDATE-RTN.                                              
      *---------------排除狀況                                        
901109     IF        ( LNL-LABOR-APPLY-CODE  NOT =  '1' )               
           THEN                                                         
                       GO TO 561-EXIT.                                  
           IF        ( WK-LABOR-DELAY-INT  =  ZEROS )                   
           THEN                                                         
                       GO TO 561-EXIT.                                  
      *---------------讀檔                                            
           EXEC        CICS  HANDLE    CONDITION                        
930730                                 NOTOPEN(911-FILE-NOTOPEN)        
"                                      ERROR  (911-FILE-OTHER-ERROR)    
060421                                 NOTFND (561-EXIT)                
                       END-EXEC.                                        
           MOVE        CTF-ACC-NO      TO   LBI-ACC-NO.                 
           MOVE        CTF-LN-ID-NO    TO   LBI-LN-ID-NO.               
           EXEC        CICS READ DATASET('LNLBINT')                     
                                 INTO   (LNLBINT)                       
                                 LENGTH (LBI-LENGTH)                    
                                 RIDFLD (LBI-KEY)                       
                                 UPDATE                                 
                       END-EXEC.                                        
      *---------------更新：未還利息餘額                              
           COMPUTE     LBI-UNPAY-INT   =    LBI-UNPAY-INT               
                                        - ( WK-LABOR-DELAY-INT          
                                          * TWA-TXN-STATUS ).           
           EXEC        CICS REWRITE    DATASET('LNLBINT')               
                                       FROM   (LNLBINT)                 
                                       LENGTH (LBI-LENGTH)              
                       END-EXEC.                                        
901109 561-EXIT.                                                        
901109     EXIT.                                                        
------*---------------ＡＧＤＩ協議利差檔處理--------------------------
001019 562-AGDI-PROC-RTN.                                               
"     *---------------控管：結案交易                                  
"          IF        ( CTF-RECV-TYPE  =  '1' OR '3' OR '5' OR '6' )     
"          THEN                                                         
"                      NEXT    SENTENCE                                 
"          ELSE                                                         
"                      GO  TO  562-EXIT.                                
001019*---------------讀檔                                            
"          MOVE        SPACES               TO   AGDI-KEY.              
"          MOVE        CTF-ACC-NO           TO   AGDI-ACC-NO.           
"          MOVE        CTF-LN-ID-NO         TO   AGDI-LN-ID-NO.         
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTOPEN(911-FILE-NOTOPEN)        
"                                      NOTFND (562-EXIT)                
"                                      DSIDERR(911-FILE-DSIDERR)        
"                                      INVREQ (911-FILE-INVREQ)         
"                                      ERROR  (911-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
060421     MOVE   '（LNAGDIF協議利差檔）'    TO   MSG-FILE-LINE2.   
001019     EXEC        CICS  READ      DATASET('LNAGDIF')               
"                                      INTO   (LNAGDIF)                 
"                                      LENGTH (AGDI-LENGTH)             
"                                      RIDFLD (AGDI-KEY)                
"                                      UPDATE                           
"                      END-EXEC.                                        
001019*---------------正常交易處理                                    
"          IF       (( NOT TWA-C-TXN-REVERSE   )                        
"                AND ( AGDI-STU-CODE  =  '0'   )                        
"                AND ( AGDI-DIFF-BAL  >  ZEROS ))                       
"                      MOVE    AGDI-STU-CODE     TO   AGDI-ORG-STU-CODE 
"                      MOVE    AGDI-L-UPD-DATE   TO   AGDI-ORG-UPD-DATE 
"                      MOVE    AGDI-TXN-ID-CODE  TO   AGDI-ORG-TXN-ID   
"                      MOVE    AGDI-RTRN-RSN     TO   AGDI-ORG-RTRN-RSN 
"                      MOVE    AGDI-DIFF-BAL     TO   AGDI-ORG-DIFF-BAL 
001019                 MOVE    AGDI-DIFF-BAL     TO   WAGDI-DIFF-BAL    
"                      MOVE    '1'               TO   AGDI-STU-CODE     
"                      MOVE    TWA-TXN-DAT7      TO   AGDI-L-UPD-DATE   
"                      MOVE    TWA-TXN-ID-CODE   TO   AGDI-TXN-ID-CODE  
"                      MOVE    CTF-CAPTL-PURPOSE TO   AGDI-RTRN-RSN     
"                      MOVE    ZEROS             TO   AGDI-DIFF-BAL     
001019*---------------沖正交易處理                                    
"          ELSE IF  (( TWA-C-TXN-REVERSE             )                  
"                AND ( AGDI-STU-CODE    NOT =  '0'   )                  
"                AND ( AGDI-DIFF-BAL        =  ZEROS )                  
"                AND ( AGDI-ORG-STU-CODE    =  '0'   )                  
"                AND ( AGDI-ORG-DIFF-BAL    >  ZEROS ))                 
"                      MOVE    AGDI-ORG-STU-CODE TO   AGDI-STU-CODE     
"                      MOVE    AGDI-ORG-UPD-DATE TO   AGDI-L-UPD-DATE   
"                      MOVE    AGDI-ORG-TXN-ID   TO   AGDI-TXN-ID-CODE  
"                      MOVE    AGDI-ORG-RTRN-RSN TO   AGDI-RTRN-RSN     
"                      MOVE    AGDI-ORG-DIFF-BAL TO   AGDI-DIFF-BAL     
001019                 MOVE    AGDI-ORG-DIFF-BAL TO   WAGDI-DIFF-BAL    
"                      MOVE    SPACES            TO   AGDI-ORG-STU-CODE 
"                      MOVE    ZEROS             TO   AGDI-ORG-UPD-DATE 
"                      MOVE    SPACES            TO   AGDI-ORG-TXN-ID   
"                      MOVE    SPACES            TO   AGDI-ORG-RTRN-RSN 
"                      MOVE    ZEROS             TO   AGDI-ORG-DIFF-BAL 
001019*---------------其他：不處理                                    
"          ELSE                                                         
"                      GO  TO  562-EXIT.                                
001019*---------------ＡＧＤＩ檔更新                                  
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTOPEN(911-FILE-NOTOPEN)        
"                                      DSIDERR(911-FILE-DSIDERR)        
"                                      INVREQ (911-FILE-INVREQ)         
"                                      ERROR  (911-FILE-OTHER-ERROR)    
"                      END-EXEC.                                        
060421     MOVE   '（LNAGDIF協議利差檔）'    TO   MSG-FILE-LINE2.   
001019     EXEC        CICS  REWRITE   DATASET('LNAGDIF')               
"                                      FROM   (LNAGDIF)                 
"                                      LENGTH (AGDI-LENGTH)             
"                      END-EXEC.                                        
"      562-EXIT.                                                        
"          EXIT.                                                        
------*----仿失業勞工寬緩---------------------------------------------
981125 570-F24-INT-HDL-RTN.                                             
           IF        ( F24-MARK  NOT =  'C' )                           
           THEN                                                         
                       GO TO 570-EXIT.                                  
           EXEC        CICS  HANDLE    CONDITION                        
                                       NOTOPEN(910-FILE-NOTOPEN)        
                                       NOTFND (910-FILE-NOTFND)         
                                       ERROR  (910-FILE-OTHER-ERROR)    
                       END-EXEC.                                        
           MOVE        CTF-ACC-NO          TO     LNF24-ACC-NO.         
           MOVE        CTF-LN-ID-NO        TO     LNF24-LN-ID-NO.       
060421     MOVE   '（ALNGVRF公教利息展延檔）' TO   MSG-FILE-LINE2.  
           EXEC        CICS READ DATASET ('ALNGVRF')                    
                                 INTO (ALNF24-REC)                      
                                 LENGTH (F24-LENGTH)                    
                                 RIDFLD (LNF24-KEY)                     
                       END-EXEC.                                        
      *---------------沖正交易：５７１算／計算利息                    
           IF        ( TWA-C-TXN-REVERSE )                              
                  OR ( LNF24-AFTER-UNPAY  =  ZEROS )                    
           THEN                                                         
                       GO TO 570-EXIT.                                  
      *---------------重設上次繳息日：若與繳息日同月份會少算一期      
           IF        ( LNL-L-RECV-INT-DATE-R  <  LNF24-DUE-DATE )       
           THEN                                                         
                       MOVE  LNL-L-RECV-INT-DATE                        
                                            TO   WKS-L-RECV-INT-DATE    
           ELSE                                                         
                       MOVE  LNF24-AFTER-UNPAY  TO  W-AMT-2             
                       GO TO 570-EXIT.                                  
           IF        ( WKS-L-RECV-INT-YY  =  LNL-RECV-INT-YY )          
                 AND ( WKS-L-RECV-INT-MM  =  LNL-RECV-INT-MM )          
           THEN                                                         
                       COMPUTE WKS-L-RECV-INT-MM =    WKS-L-RECV-INT-MM 
                                                   -  1                 
               IF    ( WKS-L-RECV-INT-MM  =  ZEROS )                    
               THEN                                                     
                       MOVE  12        TO   WKS-L-RECV-INT-MM           
                       COMPUTE WKS-L-RECV-INT-YY =    WKS-L-RECV-INT-YY 
                                                   -  1.                
           IF        ( WKS-L-RECV-INT-DD  NOT =   LNL-RECV-INT-DAY )    
           THEN                                                         
                       MOVE  LNL-RECV-INT-DAY                           
                                       TO   WKS-L-RECV-INT-DD.          
      *---------------計算本次攤還期數&應攤還利息                   
040317     MOVE        WKS-L-RECV-INT-DATE  TO   WK-DAT-BEG-DATE.       
"          MOVE        CTF-INT-END-DATE-N-R TO   WK-DAT-END-DATE        
"          MOVE        LNL-RECV-INT-DAY     TO   WK-DAT-DAYS.           
"          MOVE        01                   TO   WK-DAT-TERM-UNIT.      
"          PERFORM     815-DAT-15-CAL-RTN        THRU  815-EXIT.        
           COMPUTE     W-AMT-2             =     LNF24-AFTER-EACH       
                                           *     DAT-P-TERM-NO.         
           IF        ( W-AMT-2             >     LNF24-AFTER-UNPAY )    
                  OR ( CTF-INT-END-DATE-N-R  NOT <  LNF24-DUE-DATE )    
           THEN                                                         
                       MOVE  LNF24-AFTER-UNPAY TO W-AMT-2.              
981125 570-EXIT.                                                        
           EXIT.                                                        
981125 571-F24-UPDATE-RTN.                                              
           IF        ( F24-MARK  =  'C' OR 'Y' )                        
           THEN                                                         
                       NEXT  SENTENCE                                   
           ELSE                                                         
                       GO TO 571-EXIT.                                  
           EXEC CICS   HANDLE CONDITION                                 
"                      NOTFND (571-EXIT)                                
"                      ERROR  (571-EXIT)                                
"                      ENDFILE(571-EXIT)                                
"                      END-EXEC.                                        
           MOVE        CTF-ACC-NO          TO     LNF24-ACC-NO.         
           MOVE        CTF-LN-ID-NO        TO     LNF24-LN-ID-NO.       
           EXEC  CICS  READ                                             
                       DATASET('ALNGVRF')                               
                       INTO(ALNF24-REC)                                 
                       LENGTH(F24-LENGTH)                               
                       RIDFLD(LNF24-KEY)                                
                       UPDATE                                           
           END-EXEC.                                                    
           IF        ( TWA-C-TXN-NOT-REVERSE )                          
           THEN                                                         
               IF      LNF24-FILE-STATUS   =      9                     
               THEN                                                     
                       GO    TO   571-EXIT.                             
"          IF        ( TWA-C-TXN-REVERSE )                              
           THEN                                                         
                       MOVE  LNF24-REV-STATUS    TO  LNF24-FILE-STATUS. 
           MOVE        LNF24-FILE-STATUS    TO    LNF24-REV-STATUS.     
           MOVE        TWA-TXN-DAT7         TO    LNF24-TXN-DATE.       
           IF          LNF24-FILE-STATUS   =      0                     
                 OR    LNF24-FILE-STATUS   =      1                     
           THEN                                                         
                       MOVE    'Y'          TO    F24-MARK.             
           IF          LNF24-FILE-STATUS   =      2                     
           THEN                                                         
                       MOVE    'C'          TO    F24-MARK.             
           IF        ( F24-MARK      =  'Y' )                           
           THEN                                                         
"              IF    ( TWA-C-TXN-NOT-REVERSE )                          
               THEN                                                     
                       PERFORM  5711-F24-UPDATE-RTN THRU 5711-EXIT      
               ELSE                                                     
                       PERFORM  5712-F24-UPDATE-RTN THRU 5712-EXIT.     
           IF        ( F24-MARK      =  'C' )                           
           THEN                                                         
"              IF    ( TWA-C-TXN-NOT-REVERSE )                          
               THEN                                                     
                       PERFORM  5713-F24-UPDATE-RTN THRU 5713-EXIT      
               ELSE                                                     
                       PERFORM  5714-F24-UPDATE-RTN THRU 5714-EXIT.     
           EXEC  CICS  REWRITE                                          
                       DATASET('ALNGVRF')                               
                       FROM(ALNF24-REC)                                 
                       LENGTH(F24-LENGTH)                               
           END-EXEC.                                                    
       571-EXIT.                                                        
           EXIT.                                                        
981125 5711-F24-UPDATE-RTN.                                             
           ADD         1                TO  LNF24-ACUM-TERM.            
           IF          LNF24-TERM-NO    =   LNF24-ACUM-TERM             
           THEN                                                         
                       MOVE     2                TO  LNF24-FILE-STATUS  
                       MOVE     LNF24-LAST-AMT   TO  W-AMT-10           
                       MOVE     LNF24-LAST-INT   TO  W-AMT-8            
           ELSE                                                         
                       MOVE     1                TO  LNF24-FILE-STATUS  
                       MOVE     LNF24-EACH-AMT   TO  W-AMT-10           
                       MOVE     LNF24-EACH-INT   TO  W-AMT-8.           
           MOVE        W-AMT-10         TO  LNF24-REV-P-AMT.            
           MOVE        W-AMT-8          TO  LNF24-REV-I-AMT.            
           ADD         W-AMT-10         TO  LNF24-ACUM-AMT.             
           ADD         W-AMT-8          TO  LNF24-ACUM-INT.             
"      5711-EXIT.                                                       
"          EXIT.                                                        
981125 5712-F24-UPDATE-RTN.                                             
           SUBTRACT    1                FROM  LNF24-ACUM-TERM.          
           SUBTRACT    LNF24-REV-P-AMT  FROM  LNF24-ACUM-AMT.           
           SUBTRACT    LNF24-REV-I-AMT  FROM  LNF24-ACUM-INT.           
"      5712-EXIT.                                                       
"          EXIT.                                                        
      *----寬限期後攤還寬繳利息                                       
981125 5713-F24-UPDATE-RTN.                                             
           SUBTRACT    W-AMT-2          FROM  LNF24-AFTER-UNPAY.        
           MOVE        W-AMT-2          TO    LNF24-REV-I-AMT.          
           MOVE        ZEROS            TO    LNF24-REV-P-AMT.          
           IF          LNF24-AFTER-UNPAY   =  ZEROS                     
           THEN                                                         
                       MOVE     9         TO  LNF24-FILE-STATUS.        
"      5713-EXIT.                                                       
"          EXIT.                                                        
981125 5714-F24-UPDATE-RTN.                                             
           MOVE        2               TO     LNF24-FILE-STATUS         
           ADD         LNF24-REV-I-AMT TO     LNF24-AFTER-UNPAY.        
"      5714-EXIT.                                                       
"          EXIT.                                                        
------*---------------輸出認證----------------------------------------
       600-OUTPUT-TXN-INFO-RTN.
050614*---------------網銀交易                                        
"          IF        ( TWA-BR-CODE  =  '026' )                          
"          THEN                                                         
"                      PERFORM 670-NET-OUTPUT-RTN     THRU  670-EXIT    
"                      GO  TO  600-EXIT.                                
           IF        ( TWA-C-ID-SLIP-CH OR TWA-C-ID-SLIP-MX             
                    OR  TWA-C-ID-SLIP-TR )                              
           THEN
             PERFORM   610-SLIP-CONFORM    THRU    610-EXIT
           ELSE
             PERFORM   620-NON-SLIP-CONFORM        THRU    620-EXIT.
      *    ENDIF                                                        
891011     PERFORM   630-PRT-SCRN                  THRU    630-EXIT.    
       600-EXIT.
           EXIT.
------*-----------------------------------------------------------------
860925 601-FILE-NOT-OPEN.                                               
"             MOVE      'D205'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
"      603-DUP-REC.                                                     
"          MOVE     CTF-ACC-NO              TO      CT-WMCHK-ACC-NO.    
"          EXEC     CICS    READ            INTO(CT-WMCHK-REC)          
"                                           DATASET('ACTWMF')           
"                                           RIDFLD(CT-WMCHK-ACC-NO)     
"                                           LENGTH(CT-WMCHK-LEN)        
"                                           UPDATE                      
"          END-EXEC.                                                    
"          COMPUTE   CT-WMCHK-CR-CASH-AMT =      CT-WMCHK-CR-CASH-AMT   
"                                         +      WK-WMCHK-CR-CASH-AMT.  
980316     IF         (CT-WMCHK-CR-CASH-AMT NOT <  500000)              
"            AND       TWA-C-TXN-NOT-REENTRY                            
"            AND       TWA-C-TXN-NOT-REVERSE                            
"          THEN                                                         
"             MOVE     '#'                  TO    WK-WARNING-MARK       
"          ELSE                                                         
"             MOVE     ' '                  TO    WK-WARNING-MARK.      
"          IF          CT-WMCHK-CR-CASH-AMT <    0                      
"          THEN                                                         
"            MOVE      'D208'               TO   MSG-P-OUT-CODE         
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"     *    ENDIF                                                        
961206     IF          TWA-TXN-DAT7   NOT   =    CT-WMCHK-TXN-DATE      
"          THEN                                                         
"            MOVE      'D206'               TO   MSG-P-OUT-CODE         
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"          EXEC  CICS  REWRITE  DATASET ('ACTWMF')                      
"                      FROM (CT-WMCHK-REC)                              
"                      LENGTH (CT-WMCHK-LEN)                            
"          END-EXEC.                                                    
"          GO   TO     6101-EXIT.                                       
"      605-OTHER-ERROR.                                                 
"             MOVE      'D207'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
------*---------------通知傳票----------------------------------------
       610-SLIP-CONFORM.
860925     IF        ( TWA-C-ID-SLIP-CH OR TWA-C-ID-SLIP-MX )           
"          THEN
"                    PERFORM 6100S-MONEY-LAUNDERING-RTN THRU 6100S-EXIT.
030224     IF          TWA-TXN-ID-CODE     =       6783
           THEN
               IF   (  TWA-C-TXN-NOT-REVERSE )
               THEN
                MOVE   CTF-M-TR            TO      LOG-AMT-TYPE
                PERFORM   6100-PRNT-JOURNAL    THRU    6100-EXIT
                PERFORM   611-ATM-FILE-RTN    THRU    611-EXIT
                GO   TO   610-EXIT.
      *    ENDIF.
           MOVE        'W610'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           MOVE        'T638'              TO      MSG-P-OUT-CODE.
           MOVE        MSG-M-DOCUMENT      TO      MSG-P-OUT-TM-TYPE.   
B00309     MOVE        228                 TO      MSG-P-LENGTH.        
           PERFORM     690-AMT-TYPE-CHK    THRU    690-EXIT.
           MOVE        TWA-TM-CODE         TO      MSG-P-SP-TM-CODE.
           MOVE        TWA-ACC-TXN-SEQ-NO  TO      MSG-P-SP-TXN-SEQ-NO.
           MOVE        TWA-TXN-ID-CODE     TO      MSG-P-SP-TXN-ID-CODE.
           MOVE        SPACES              TO      W-MSG-DATA2.
961206     MOVE        TWA-TXN-DAT7        TO      W-MSG2-TXN-DATE.     
"          MOVE        TWA-TXN-TIME-HH     TO      W-MSG2-TXN-TIME-HH.  
"          MOVE        ':'                 TO      W-MSG2-FILLER1.      
"          MOVE        TWA-TXN-TIME-MM     TO      W-MSG2-TXN-TIME-MM.  
"          MOVE        ':'                 TO      W-MSG2-FILLER2.      
"          MOVE        TWA-TXN-TIME-SS     TO      W-MSG2-TXN-TIME-SS.  
"          MOVE        TWA-TELLER-CODE     TO      W-MSG2-TELLER-CODE.  
080116     MOVE        'TELNAME   '         TO   T638-TELLER-NAME.      
           MOVE        CTF-ADVICE-INFO     TO      W-MSG2-ACC-NO.
           MOVE        ALL '*'             TO      W-MSG2-LN-ID-NO-X.
           MOVE        2                   TO      W-MSG2-RECV-TYPE.
"          MOVE        CTF-INT-BGN-DATE-N-R  TO    W-MSG2-INT-BGN-DATE. 
900706     MOVE      CTF-INT-END-DATE-N-R  TO      W-MSG2-INT-END-DATE. 
           MOVE        CTF-RECV-AMT        TO      W-MSG2-RECV-AMT.
           MOVE        CTF-FINE            TO      W-MSG2-FINE.
060919     MOVE        CTF-INT-AMT          TO   W-MSG2-TOTAL-INT-AMT.  
           IF        ( TWA-C-SUP-OFF )                                  
080116            OR ( CTF-SUP-CODE  =  ZEROS )                         
           THEN
                       MOVE  '**'                TO   W-MSG2-SUP-CODE-X 
080116                 MOVE  SPACES              TO   T638-SUP-NAME     
           ELSE
                       MOVE  CTF-SUP-CODE        TO   W-MSG2-SUP-CODE   
080116                 MOVE  'SUPNAME   '        TO   T638-SUP-NAME.    
           IF          (W-DIFF NOT > 0)
           THEN
             MOVE      W-DIFF              TO    W-MSG2-DIFF-PLUS
             MOVE      W-MSG2-DIFF-PLUS    TO    W-MSG2-RECV-RTRN-DIFF.
           IF          (W-DIFF > 0)
           THEN
             COMPUTE   W-DIFF               =    W-DIFF * -1
             MOVE      W-DIFF              TO    W-MSG2-DIFF-MINUS
             MOVE      W-MSG2-DIFF-MINUS   TO    W-MSG2-RECV-RTRN-DIFF.
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      W-MSG2-150-MARK.     
"          MOVE        CTF-CAPTL-PURPOSE    TO   W-MSG2-REASON.         
980518     IF        ( CTF-RECV-TYPE  NOT NUMERIC )                     
"                OR  ( CTF-RECV-TYPE  =  ZEROS )                        
"          THEN                                                         
"                      MOVE    '**'         TO   W-MSG2-REASON.         
"          MOVE        SPACES              TO      W-MSG2-FAX-NO.       
050822     MOVE        SPACES              TO      W-MSG2-SCH-RTRN.     
"          IF        ( LNP-LN-CODE    =  55 )
"               AND  ( LNL-ACC-CODE   =  37 )
"               AND  ( LNL-LN-BAL     =  ZEROS )
"          THEN
"                      MOVE '(本筆還清)' TO      W-MSG2-SCH-RTRN.
      *    END-IF.
B00309     MOVE        SPACES              TO      W-MSG2-AML-DATA.     
           MOVE        W-MSG-DATA2         TO      MSG-P-SP-OTHER-DATA.
           CALL        'UCPWRITE'          USING   TWA MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
060113*---------------學貸清償：收據聯列印『還清』字句                
"          PERFORM     610A-SCH-CLS-OUT2-RTN     THRU  610A-EXIT.       
      *---------------洗錢防制法／※必須在ＪＯＵＲＮＡＬ前面          
860925     IF          WK-WARNING-MARK     =     '#'                    
"          THEN                                                         
"             MOVE      MSG-M-DISPLAY       TO      MSG-P-OUT-TM-TYPE   
"             MOVE     'T6J8'               TO      MSG-P-OUT-CODE      
"             MOVE      40                  TO      MSG-P-LENGTH        
"             MOVE      MSG-XXX1            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX2            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX3            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX4            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX5            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX6            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX7            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG3     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG1     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG2     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE     'T6J7'              TO      MSG-P-OUT-CODE       
"             MOVE      84                 TO      MSG-P-LENGTH         
"             MOVE      MSG-M-DOCUMENT     TO      MSG-P-OUT-TM-TYPE    
"             MOVE      ALL  '#'           TO      WK-M-L-AMT-TYPE      
"             MOVE      ALL  '#'           TO      WK-M-L-SEQ-NO        
"             MOVE      TWA-TXN-ID-CODE    TO      WK-M-L-TXN-ID-CODE   
"             MOVE      CTF-ACC-NO         TO      WK-M-L-ACC-ID-NO     
"             MOVE      CTF-LN-ID-NO       TO      WK-M-L-LN-ID-NO      
"             MOVE      WK-WMCHK-CR-CASH-AMT TO    WK-M-L-TXN-AMT       
"             MOVE      CT-WMCHK-CR-CASH-AMT TO    WK-M-L-CASH-AMT      
"             MOVE      ALL '*'            TO      WK-M-L-SUP-KEY       
961206        MOVE      TWA-TXN-DAT7       TO      WK-M-L-DATE          
"             MOVE      WK-WARNING-MARK    TO      WK-M-L-FILLER3       
"             MOVE      WK-M-LAUNDERING     TO   MSG-P-OUT-MSG(1:85)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
041211        PERFORM   6102-SEND-920-DATA       THRU  6102-EXIT.       
 7908      PERFORM   6100-PRNT-JOURNAL    THRU    6100-EXIT.            
       610-EXIT.
           EXIT.
------*---------------學貸清償－收據聯列印『還清』字句----------------
060113*---------------為減少行數設段落以利共用------------------------
"      610A-SCH-CLS-OUT2-RTN.                                           
050720     IF        ( LNP-LN-CODE    =  55 )                           
                AND  ( LNL-ACC-CODE   =  37 )                           
                AND  ( LNL-LN-BAL     =  ZEROS )                        
           THEN                                                         
                       NEXT    SENTENCE                                 
           ELSE                                                         
                       GO  TO  610A-EXIT.                               
051014*---------------就學貸款排除特定工作站                          
B00101     IF        ( TWA-TM-ID-NO  =  '10181' )                       
"          THEN                                                         
"                      GO  TO  610A-EXIT.                               
      *---------------用紙提示                                        
           MOVE        'W631'               TO   MSG-P-OUT-CODE.        
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
      *---------------空３行                                          
           MOVE        SPACES               TO   MSG-P-OUT.             
           MOVE        'T6J3'               TO   MSG-P-OUT-CODE.        
           MOVE        85                   TO   MSG-P-LENGTH.          
           MOVE        MSG-M-DOCUMENT       TO   MSG-P-OUT-TM-TYPE.     
           CALL        'UCPWRITE'       USING   TWA MSG-P-OUT.          
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
           CALL        'UCPWRITE'       USING   TWA MSG-P-OUT.          
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
           CALL        'UCPWRITE'       USING   TWA MSG-P-OUT.          
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
      *---------------輸出還清字句                                    
           MOVE        '(本筆還清)'      TO   MSG-P-OUT-MSG(64:12).   
           CALL        'UCPWRITE'       USING   TWA MSG-P-OUT.          
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
060113 610A-EXIT.                                                       
"           EXIT.                                                       
------*-----------------------------------------------------------------
030224 COPY  LNK67C1.
------*---------------ＪＯＵＲＮＡＬ----------------------------------
       6100-PRNT-JOURNAL.
           MOVE        SPACE               TO      WS-MSG-DATA-JOURNAL.
           MOVE        TWA-TXN-TIME-HH     TO      WJ-MSG-HH.
           MOVE        ':'                 TO      WJ-MSG-F1.
           MOVE        TWA-TXN-TIME-MM     TO      WJ-MSG-MM.
           MOVE        ':'                 TO      WJ-MSG-F2.
           MOVE        TWA-TXN-TIME-SS     TO      WJ-MSG-SS.
           MOVE        CTF-ADVICE-INFO     TO      WJ-MSG-ACC-NO.
           MOVE        ALL '*'             TO      WJ-MSG-LN-ID-NO.     
           MOVE        2                   TO      WJ-MSG-COR-TYPE.
900706     MOVE        CTF-INT-END-DATE-N-R  TO    WJ-MSG-DATE.         
           MOVE        CTF-RECV-AMT        TO      WJ-MSG-AMT.
           IF          TWA-C-SUP-OFF
           THEN
040317       MOVE      '**'                TO      WJ-MSG-SUP-CODEX     
           ELSE
             MOVE      CTF-SUP-CODE        TO      WJ-MSG-SUP-CODE.
           IF          TWA-C-TXN-REVERSE
                  AND  CTF-SUP-CODE        =       ZERO
           THEN
040317       MOVE      '**'                TO      WJ-MSG-SUP-CODEX.    
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      WJ-MSG-150-MARK.     
  "        MOVE        83                  TO      MSG-P-LENGTH.
           MOVE        MSG-M-JOURNAL       TO      MSG-P-OUT-TM-TYPE.
           MOVE        'T6J2'              TO      MSG-P-OUT-CODE.
           CALL        'UCPWRITE'          USING   TWA, MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
       6100-EXIT.
           EXIT.
------*-----------------------------------------------------------------
041211*大額通貨交易送〔帳號／ＩＤ／客戶序號〕至端末系統，             
"     *供啟動〔９２００交易〕並將資料填入畫面輸入項目使用             
"     *-----------------------------------------------------------------
041211 6102-SEND-920-DATA.                                              
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
"          MOVE        'T105'               TO   MSG-P-OUT-CODE.        
"          MOVE        98                   TO   MSG-P-LENGTH.          
"          MOVE        'C'                  TO   MSG-P-OUT-TM-TYPE.     
"          MOVE        CTF-ACC-NO           TO   MSG-P-T105-ACNO.       
041211*---------------設定：客戶ＩＤ／序號                            
"          MOVE        WLNM-CUST-ID         TO   MSG-P-T105-PER-ID.     
"          MOVE        WK-LNM-CFM-SER       TO   MSG-P-T105-SER.        
"          PERFORM     935-UCPWRITE-HDL-RTN.                            
"      6102-EXIT.                                                       
"           EXIT.                                                       
------*-----------------------------------------------------------------
       620-NON-SLIP-CONFORM.
           MOVE        'W610'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           PERFORM     621-PRINT-RECV-SLIP THRU    621-EXIT.
 7908      PERFORM   6210-PRNT-JOURNAL    THRU    6210-EXIT.
860925     IF          WK-WARNING-MARK     =     '#'                    
"          THEN                                                         
"             MOVE      MSG-M-DISPLAY       TO      MSG-P-OUT-TM-TYPE   
"             MOVE     'T6J8'               TO      MSG-P-OUT-CODE      
"             MOVE      40                  TO      MSG-P-LENGTH        
"             MOVE      MSG-XXX1            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX2            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX3            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX4            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX5            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX6            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      MSG-XXX7            TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG3     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG1     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE      WK-WARNING-MSG2     TO   MSG-P-OUT-MSG(1:40)    
"             PERFORM   935-UCPWRITE-HDL-RTN                            
"             MOVE     'T6J7'              TO      MSG-P-OUT-CODE       
"             MOVE      84                 TO      MSG-P-LENGTH         
"             MOVE      MSG-M-DOCUMENT     TO      MSG-P-OUT-TM-TYPE    
"             MOVE      ALL  '#'           TO      WK-M-L-AMT-TYPE      
"             MOVE      ALL  '#'           TO      WK-M-L-SEQ-NO        
"             MOVE      TWA-TXN-ID-CODE    TO      WK-M-L-TXN-ID-CODE   
"             MOVE      CTF-ACC-NO         TO      WK-M-L-ACC-ID-NO     
"             MOVE      CTF-LN-ID-NO       TO      WK-M-L-LN-ID-NO      
"             MOVE      WK-WMCHK-CR-CASH-AMT TO    WK-M-L-TXN-AMT       
"             MOVE      CT-WMCHK-CR-CASH-AMT TO    WK-M-L-CASH-AMT      
"             MOVE      ALL '*'            TO      WK-M-L-SUP-KEY       
961206        MOVE      TWA-TXN-DAT7       TO      WK-M-L-DATE          
"             MOVE      WK-WARNING-MARK    TO      WK-M-L-FILLER3       
141229*       MOVE      WK-M-LAUNDERING     TO   MSG-P-OUT(1:85)        
141229        MOVE      WK-M-LAUNDERING     TO   MSG-P-OUT-MSG(1:85)    
              PERFORM   935-UCPWRITE-HDL-RTN                            
041211        PERFORM   6102-SEND-920-DATA       THRU  6102-EXIT.       
      *    ENDIF                                                        
B00309     PERFORM      6103-NO-REVERSE-CFMJIB5 THRU 6103-EXIT.         
       620-EXIT.
           EXIT.
------*---------------認證傳票（非通知傳票交易）----------------------
       621-PRINT-RECV-SLIP.
860925     IF        ( TWA-C-ID-RCVHI-CH OR TWA-C-ID-RCVHI-MX )         
"          THEN
"                    PERFORM 6100S-MONEY-LAUNDERING-RTN THRU 6100S-EXIT.
           MOVE        'T638'              TO      MSG-P-OUT-CODE.
           MOVE        MSG-M-DOCUMENT      TO      MSG-P-OUT-TM-TYPE.   
B00309     MOVE        261                 TO      MSG-P-LENGTH.        
           PERFORM     690-AMT-TYPE-CHK    THRU    690-EXIT.
           MOVE        TWA-TM-CODE         TO      MSG-P-SP-TM-CODE.
           MOVE        TWA-ACC-TXN-SEQ-NO  TO      MSG-P-SP-TXN-SEQ-NO.
           MOVE        TWA-TXN-ID-CODE     TO      MSG-P-SP-TXN-ID-CODE.
           MOVE        SPACES              TO      W-MSG-DATA2.
961206     MOVE        TWA-TXN-DAT7        TO      W-MSG2-TXN-DATE.     
"          MOVE        TWA-TXN-TIME-HH     TO      W-MSG2-TXN-TIME-HH.  
"          MOVE        ':'                 TO      W-MSG2-FILLER1.      
"          MOVE        TWA-TXN-TIME-MM     TO      W-MSG2-TXN-TIME-MM.  
"          MOVE        ':'                 TO      W-MSG2-FILLER2.      
"          MOVE        TWA-TXN-TIME-SS     TO      W-MSG2-TXN-TIME-SS.  
"          MOVE        TWA-TELLER-CODE     TO      W-MSG2-TELLER-CODE.  
080116     MOVE        'TELNAME   '         TO   T638-TELLER-NAME.      
           MOVE        CTF-ACC-NO          TO      W-MSG2-ACC-NO.
           MOVE        CTF-LN-ID-NO        TO      W-MSG2-LN-ID-NO.
           MOVE        CTF-RECV-TYPE       TO      W-MSG2-RECV-TYPE.
970706     MOVE        CTF-INT-BGN-DATE-N-R  TO    W-MSG2-INT-BGN-DATE. 
"          MOVE      CTF-INT-END-DATE-N-R  TO      W-MSG2-INT-END-DATE. 
           MOVE        CTF-RECV-AMT        TO      W-MSG2-RECV-AMT.
           MOVE        CTF-FINE            TO      W-MSG2-FINE.
110822     MOVE        CTF-INT-AMT          TO   W-MSG2-TOTAL-INT-AMT.  
           IF        ( TWA-C-SUP-OFF )                                  
080116            OR ( CTF-SUP-CODE  =  ZEROS )                         
           THEN                                                         
                       MOVE  '**'                TO   W-MSG2-SUP-CODE-X 
080116                 MOVE  SPACES              TO   T638-SUP-NAME     
           ELSE                                                         
                       MOVE  CTF-SUP-CODE        TO   W-MSG2-SUP-CODE   
080116                 MOVE  'SUPNAME   '        TO   T638-SUP-NAME.    
           IF          (W-DIFF NOT > 0)
           THEN
             MOVE      W-DIFF              TO    W-MSG2-DIFF-PLUS
             MOVE      W-MSG2-DIFF-PLUS    TO    W-MSG2-RECV-RTRN-DIFF.
           IF          (W-DIFF > 0)
           THEN
             COMPUTE   W-DIFF               =    W-DIFF * -1
             MOVE      W-DIFF              TO    W-MSG2-DIFF-MINUS
             MOVE      W-MSG2-DIFF-MINUS   TO    W-MSG2-RECV-RTRN-DIFF.
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      W-MSG2-150-MARK.     
920903     MOVE        CTF-CAPTL-PURPOSE    TO   W-MSG2-REASON.         
981223     IF          TWA-TXN-ID-CODE      =      6770                 
"          THEN                                                         
"            IF        CTF-BUSINS-CODE      =      ZEROS                
"            THEN                                                       
"              MOVE    SPACES              TO      W-MSG2-FAX-NO        
"            ELSE                                                       
"              MOVE    CTF-BUSINS-CODE     TO      W-MSG2-FAX-NO        
"          ELSE                                                         
"            MOVE      SPACES              TO      W-MSG2-FAX-NO.       
                                                                        
050720     MOVE        SPACES              TO      W-MSG2-SCH-RTRN.     
           IF        ( LNP-LN-CODE    =  55 )
                AND  ( LNL-ACC-CODE   =  37 )
                AND  ( LNL-LN-BAL     =  ZEROS )
           THEN
                       MOVE '(本筆還清)' TO      W-MSG2-SCH-RTRN.
      *****END-IF.                                                      
B00309     MOVE        SPACES              TO      W-MSG2-AML-DATA.     
B00309     IF          TWA-C-ID-RCVIN-CH OR TWA-C-ID-RCVHI-CH           
B00309              OR TWA-C-ID-RCVIN-MX OR TWA-C-ID-RCVHI-MX           
           THEN                                                         
                       PERFORM  62111-LAUNDER-SLIP THRU  62111-EXIT.    
           MOVE        W-MSG-DATA2         TO      MSG-P-SP-OTHER-DATA.
           CALL        'UCPWRITE'          USING   TWA MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
060113*---------------學貸清償：收據聯列印『還清』字句                
"          PERFORM     610A-SCH-CLS-OUT2-RTN     THRU  610A-EXIT.       
       621-EXIT.
           EXIT.
       62111-LAUNDER-SLIP.                                              
           IF        ( WK-WARNING-MARK  NOT =   '#' )                   
           THEN                                                         
                       GO   TO  62111-EXIT.                             
           IF          WS-BLACK-LIST       =   'Y'                      
           THEN                                                         
                       MOVE  '經比對非黑名單' TO  W-MSG2-AML-BLACK    
           ELSE                                                         
                       MOVE  '查無黑名單資料' TO  W-MSG2-AML-BLACK.   
           MOVE  'ＡＭＬ案件編號:'            TO  W-MSG2-AML-CODE-C.  
           MOVE        WS-AML-CODE             TO  W-MSG2-AML-CODE.     
           MOVE        ','             TO  W-MSG2-AML-BLACK-COMM.       
           IF          WS-AML-CODE     =   SPACES                       
                AND    WS-BLACK-LIST   =   SPACES                       
                AND    WS-TXN-CUST-NAME NOT = SPACES                    
           THEN                                                         
                       MOVE   SPACES   TO      W-MSG2-AML-DATA          
                       MOVE  '發查ＡＭＬ失敗' TO  W-MSG2-AML-BLACK.   
       62111-EXIT.                                                      
           EXIT.                                                        
------*-----------------------------------------------------------------
860925 6100S-MONEY-LAUNDERING-RTN.                                      
"          IF          (TWA-C-ID-RCVHI-CH)                              
"                OR    (TWA-C-ID-SLIP-CH)                               
"          THEN                                                         
"                COMPUTE WK-WMCHK-CR-CASH-AMT  =  WK-WMCHK-CR-CASH-AMT  
"                                        + (((CTF-RECV-AMT)             
"                                        +  (CTF-INT-AMT)               
"                                        +  (CTF-FINE))                 
"                                        *  TWA-TXN-STATUS).            
"          IF          (TWA-C-ID-RCVHI-MX)                              
"                OR    (TWA-C-ID-SLIP-MX)                               
"          THEN                                                         
"                COMPUTE WK-WMCHK-CR-CASH-AMT  =  WK-WMCHK-CR-CASH-AMT  
"                                              +  (CTF-RECV-CASH-AMT    
"                                              *  TWA-TXN-STATUS).      
"          MOVE        CTF-ACC-NO          TO      CT-WMCHK-ACC-NO.     
"          IF       ( LNM-TAX-ID  =  SPACES OR ZEROS )                  
"          THEN                                                         
"                    MOVE   LNM-CUST-ID  TO      CT-WMCHK-ID-NO         
"          ELSE                                                         
"                    MOVE   '00'         TO      CT-WMCHK-ID-NO1        
"                    MOVE   LNM-TAX-ID   TO      CT-WMCHK-ID-NO2.       
"          MOVE      ZEROS                 TO    CT-WMCHK-DR-CASH-AMT.  
"          MOVE      WK-WMCHK-CR-CASH-AMT  TO    CT-WMCHK-CR-CASH-AMT.  
961206     MOVE      TWA-TXN-DAT7        TO      CT-WMCHK-TXN-DATE.     
980316     IF          WK-WMCHK-CR-CASH-AMT NOT  <     500000           
"            AND       TWA-C-TXN-NOT-REENTRY                            
"            AND   NOT TWA-C-TXN-REVERSE                                
"          THEN                                                         
"             MOVE     '#'                  TO      WK-WARNING-MARK.    
"          PERFORM  6101-PROCED-WMCHK-RTN  THRU  6101-EXIT.             
"      6100S-EXIT.                                                      
"          EXIT.                                                        
------*-----------------------------------------------------------------
"      6101-PROCED-WMCHK-RTN.                                           
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN(601-FILE-NOT-OPEN)                       
"                      DUPREC(603-DUP-REC)                              
"                      ERROR(605-OTHER-ERROR)                           
"                      END-EXEC.                                        
"          IF          TWA-C-TXN-REVERSE                                
"          THEN                                                         
"            GO  TO    603-DUP-REC.                                     
"          EXEC  CICS  WRITE  DATASET ('ACTWMF')                        
"                      RIDFLD (CT-WMCHK-ACC-NO)                         
"                      FROM (CT-WMCHK-REC)                              
"                      LENGTH (CT-WMCHK-LEN)                            
"          END-EXEC.                                                    
"      6101-EXIT.                                                       
"          EXIT.                                                        
*-----*------------/寫入ＣＦＭＪＩＢ５檔/----------------------------*
B00309 6103-NO-REVERSE-CFMJIB5.                                         
           IF          TWA-C-TXN-REVERSE       GO  TO  6103-EXIT.       
           IF   NOT    WK-WARNING-MARK = '#'   GO TO 6103-EXIT.         
************交易人為個人且為帳戶本人時,不必執行姓名檢核             
           IF          CTF-AML-CODE    =   SPACES GO TO 6103-EXIT.      
           EXEC      CICS    HANDLE    CONDITION                        
                                       NOTOPEN (6103-NOTOPEN)           
                                       DUPREC  (6103-DUPREC)            
                                       ERROR   (6103-ERROR)             
                     END-EXEC.                                          
           MOVE      SPACES                  TO   MJIB5-REC.            
           MOVE      CTF-AML-CODE            TO  MJIB5-AML-NO.          
           MOVE      CTF-ACC-NO              TO  MJIB5-ACC-NO.          
           MOVE      TWA-BR-CODE             TO  MJIB5-BR.              
           MOVE      TWA-TXN-DAT7            TO  MJIB5-DAY.             
           MOVE      TWA-TXN-ID-CODE         TO  MJIB5-TXN-CODE.        
           MOVE      CTF-W-LOG-AMT           TO  MJIB5-TXN-AMT.         
B00309     IF       ( TWA-TXN-ID-CODE-1 = 671 OR 672)                   
"          THEN                                                         
"            MOVE    CTF-W-LOG-AMT           TO  MJIB5-TXN-AMT.         
"          IF       ( TWA-TXN-ID-CODE-1 = 674 OR 675 )                  
"          THEN                                                         
"            MOVE    CTF-RECV-CASH-AMT       TO  MJIB5-TXN-AMT.         
           MOVE      CTF-PAY-NAME            TO  MJIB5-TXN-NAME.        
           MOVE      TWA-TXN-TIME-HH         TO  WS-TIME-HH.            
           MOVE      TWA-TXN-TIME-MM         TO  WS-TIME-MM.            
           MOVE      WS-TIME-HHMM            TO  MJIB5-TIME.            
           EXEC      CICS    WRITE     DATASET ('CFMJIB5')              
                                       FROM    (MJIB5-REC)              
                                       RIDFLD  (MJIB5-KEY)              
                                       LENGTH  (MJIB5-LEN)              
                     END-EXEC.                                          
           GO   TO   6103-EXIT.                                         
       6103-NOTOPEN.                                                    
           MOVE     'D802'                   TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
       6103-DUPREC.                                                     
           MOVE     'D179'                   TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
       6103-ERROR.                                                      
           MOVE     'D804'                   TO  MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
       6103-EXIT.                                                       
           EXIT.                                                        
------*-----------------------------------------------------------------
       6210-PRNT-JOURNAL.
           MOVE        SPACE               TO      WS-MSG-DATA-JOURNAL.
           MOVE        TWA-TXN-TIME-HH     TO      WJ-MSG-HH.
           MOVE        TWA-TXN-TIME-MM     TO      WJ-MSG-MM.
           MOVE        TWA-TXN-TIME-SS     TO      WJ-MSG-SS.           
           MOVE        ':'                 TO    WJ-MSG-F1 WJ-MSG-F2.   
           MOVE        CTF-ACC-NO          TO      WJ-MSG-ACC-NO.
           MOVE        CTF-LN-ID-NO        TO      WJ-MSG-LN-ID-NO.
           MOVE        CTF-RECV-TYPE       TO      WJ-MSG-COR-TYPE.
900706     MOVE        CTF-INT-END-DATE-N-R  TO    WJ-MSG-DATE.         
           MOVE        CTF-RECV-AMT        TO      WJ-MSG-AMT.
           IF          TWA-C-SUP-OFF
           THEN
040317       MOVE      '**'                TO      WJ-MSG-SUP-CODEX     
           ELSE
             MOVE      CTF-SUP-CODE        TO      WJ-MSG-SUP-CODE.
           IF          TWA-C-TXN-REVERSE
                  AND  CTF-SUP-CODE        =       ZERO
           THEN
040317       MOVE      '**'                TO      WJ-MSG-SUP-CODEX.    
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      WJ-MSG-150-MARK.     
860925     MOVE        83                  TO      MSG-P-LENGTH.        
           MOVE        MSG-M-JOURNAL       TO      MSG-P-OUT-TM-TYPE.
           MOVE        'T6J2'              TO      MSG-P-OUT-CODE.
           CALL        'UCPWRITE'          USING   TWA, MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
       6210-EXIT.
           EXIT.
------*-----------------------------------------------------------------
891011 630-PRT-SCRN.                                                    
             IF        W-CTF-C-RTRN                                     
                 NEXT     SENTENCE                                      
             ELSE                                                       
               GO   TO   630-EXIT.                                      
"            IF        LNL-ACC-CODE   =    25                           
"            AND       LNP-LN-CODE    =    41                           
"            THEN                                                       
                 NEXT     SENTENCE                                      
               ELSE                                                     
                 GO   TO   630-EXIT.                                    
"             MOVE      MSG-M-DISPLAY       TO      MSG-P-OUT-TM-TYPE.  
"             MOVE     'T6J3'               TO      MSG-P-OUT-CODE.     
"             MOVE      85                  TO      MSG-P-LENGTH.       
"             MOVE      WK-MSG-PB-MARK      TO   MSG-P-OUT-MSG(1:85).   
"             PERFORM   935-UCPWRITE-HDL-RTN.                           
       630-EXIT.                                                        
           EXIT.                                                        
------*---------------提前清償警語提示--------------------------------
031215 640-PRECLS-WARN-RTN.                                             
      *---------------設定：限制清償截止日                            
141124*---------------【１房貸】２年                                  
"          MOVE        LNL-1ST-LN-DATE      TO   WK-WORK-DATE.          
"          IF        ( LNP-LMT-RTN-MARK  =  '1' )                       
"                      ADD     2                 TO   WK-WORK-YY        
141124*---------------【２太陽光電】３年                              
"          ELSE IF   ( LNP-LMT-RTN-MARK  =  '2' )                       
"                      ADD     3                 TO   WK-WORK-YY        
141124*---------------【３客製限制清償】讀ＬＭＴＲ檔設定相關參數      
"          ELSE IF   ( LNP-LMT-RTN-MARK  =  '3' )                       
"                AND ( WLMTR-READ-MARK   =  'Y' )                       
"                      MOVE    WLMTR-END-DATE    TO   WK-WORK-DATE      
"          ELSE                                                         
"                      GO  TO  640-EXIT                                 
"          END-IF.                                                      
031215*---------------提前清償警語提示                                
"          IF        ( TWA-TXN-DAT7  NOT >  WK-WORK-DATE )              
050614           AND ( TWA-BR-CODE   NOT =  '026'   )                   
"          THEN                                                         
031215                 MOVE  SPACES              TO   MSG-T282          
"            MOVE '本案係限制清償期間案件，'   TO   MSG-T282-LINE1    
"            MOVE '如申請抵押權塗銷者須另收取違約金，'                
"                                                TO   MSG-T282-LINE2    
"            MOVE '違約金額請洽放款人員。'     TO   MSG-T282-LINE3    
"                      MOVE  'T282'              TO   MSG-P-OUT-CODE    
"                      MOVE  208                 TO   MSG-P-LENGTH      
"                      MOVE  MSG-M-DISPLAY       TO   MSG-P-OUT-TM-TYPE 
"                      CALL  'UCPWRITE'       USING   TWA  MSG-P-OUT    
"          END-IF.                                                      
"      640-EXIT.                                                        
"          EXIT.                                                        
------*---------------輸出：認證存摺轉帳傳票--------------------------
931216 660-PRT-PB-SLIP.                                                 
           MOVE       'W624'               TO     MSG-P-OUT-CODE.       
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
010426*---------------〔轉支傳票〕填寫欄（沖正不印）                  
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
"          THEN                                                         
"                      PERFORM 661-T6A4-OUTPUT-RTN    THRU  661-EXIT.   
010426*---------------〔轉支傳票〕認證欄                              
"          MOVE        'T6A5'               TO   MSG-P-OUT-CODE.        
"          MOVE        MSG-M-DOCUMENT       TO   MSG-P-OUT-TM-TYPE.     
080116     MOVE        180                  TO   MSG-P-LENGTH.          
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
"     *---------------第１行                                          
"          IF        ( TWA-C-TXN-NOT-REVERSE )                          
"          THEN                                                         
"                      MOVE    CTF-M-TR          TO   T6A5-ACC-TYPE     
"          ELSE                                                         
"                      MOVE    CTF-M-TR-REVERSE  TO   T6A5-ACC-TYPE.    
      *****ENDIF                                                        
010426     MOVE        TWA-TM-CODE          TO   T6A5-TM-NO.            
"          MOVE        WK-ACC-TXN-SEQ-NO    TO   T6A5-TXN-SER-NO.       
021210     MOVE        '1483'               TO   T6A5-TXN-CODE.         
"          MOVE        CTF-TR-ACC-NO-1      TO   T6A5-ACC-NO1.          
"          MOVE        '放款繳本息　'     TO   T6A5-TR-MEMO.          
010426     MOVE        CTF-W-LOG-AMT        TO   T6A5-TR-AMT.           
"          MOVE        TWA-TXN-DAT7         TO   T6A5-OUT-DATE.         
"          MOVE        SPACES               TO   T6A5-TR-CODE.          
"          MOVE        ALL   '*'            TO   T6A5-TR-PB-BAL.        
080116*---------------第２行                                          
"          MOVE        TWA-TELLER-CODE      TO   T6A5-TELLER-CODE.      
"          MOVE        'TELNAME   '         TO   T6A5-TELLER-NAME.      
010426     IF        ( TWA-C-SUP-OFF )                                  
"          THEN                                                         
"                      MOVE   ALL '*'            TO   T6A5-SUP-CODE     
080116                 MOVE   '　　　　'       TO   T6A5-SUP-NAME     
"          ELSE                                                         
"                      MOVE   CTF-SUP-CODE       TO   T6A5-SUP-CODE     
080116                 MOVE   'SUPNAME   '       TO   T6A5-SUP-NAME.    
      *****ENDIF                                                        
080116     MOVE      TWA-TXN-TIME-HH        TO   T6A5-TIME-HH.          
"          MOVE      TWA-TXN-TIME-MM        TO   T6A5-TIME-MM.          
"          MOVE      ':'               TO   T6A5-TIME-X1 T6A5-TIME-X2.  
"          MOVE      TWA-TXN-TIME-SS        TO   T6A5-TIME-SS.          
010426*---------------列印                                            
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
931216 660-EXIT.                                                        
           EXIT.                                                        
------*---------------設定：〔轉支傳票〕上半部（填寫欄）--------------
010426 661-T6A4-OUTPUT-RTN.                                             
"     *---------------參數設定                                        
"          MOVE        'T6A4'               TO   MSG-P-OUT-CODE.        
"          MOVE        197                  TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-DOCUMENT       TO   MSG-P-OUT-TM-TYPE.     
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
010426*---------------會計科目代碼名稱／交易日                        
"          PERFORM     6610-T6A4-ACCT-RTN        THRU  6610-EXIT.       
"          INSPECT     T6A4-ACCT-NAME  REPLACING ALL  X'0E' BY SPACES.  
"          INSPECT     T6A4-ACCT-NAME  REPLACING ALL  X'0F' BY SPACES.  
"          MOVE        X'0E'                TO   T6A4-ACCT-0E.          
"          MOVE        X'0F'                TO   T6A4-ACCT-0F.          
"          MOVE        TWA-TXN-YY7          TO   T6A4-TXN-YY.           
"          MOVE        TWA-TXN-MM7          TO   T6A4-TXN-MM.           
"          MOVE        TWA-TXN-DD7          TO   T6A4-TXN-DD.           
010426*---------------會計子目代碼名稱                                
"          MOVE        '0000'               TO   T6A4-SUB-CODE.         
"          MOVE        SPACES               TO   T6A4-SUB-NAME.         
"          MOVE        X'0E'                TO   T6A4-SUB-0E.           
"          MOVE        X'0F'                TO   T6A4-SUB-0F.           
010426*---------------帳號／戶名／交易金額                            
"          MOVE        CTF-TR-ACC-NO-1      TO   T6A4-ACC-NO.           
"          MOVE        SV-CUST-NAME         TO   T6A4-CUST-NAME.        
"          INSPECT     T6A4-CUST-NAME  REPLACING ALL  X'0E' BY SPACE.   
"          INSPECT     T6A4-CUST-NAME  REPLACING ALL  X'0F' BY SPACE.   
"          MOVE        X'0E'                TO   T6A4-CUST-0E.          
"          MOVE        X'0F'                TO   T6A4-CUST-0F.          
           MOVE        CTF-W-LOG-AMT        TO   T6A4-TXN-AMT.          
010426*---------------摘要１／摘要２（帳號授信號碼）                  
"          MOVE   '委託代繳放款本息'      TO   T6A4-MEMO-1.           
"          MOVE        CTF-ACC-NO           TO   T6A4-LN-ACC-NO.        
"          MOVE        CTF-APRV-NO          TO   T6A4-APRV-NO.          
"          MOVE        '-'                  TO   T6A4-DASH1.            
"          MOVE        CTF-LN-SER-NO        TO   T6A4-LN-SER-NO.        
030530*---------------摘要３／大額換人驗印警示                        
"          MOVE        SPACES               TO   T6A4-MEMO-3.           
"          IF        ( CTF-W-LOG-AMT  NOT <  5000000 )                  
"          THEN                                                         
"                      MOVE '本交易請換人驗印' TO   T6A4-MEMO-3.      
"     *    ENDIF                                                        
010426*---------------列印                                            
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
"      661-EXIT.                                                        
"          EXIT.                                                        
------*---------------設定：會計科目代碼名稱（存款連動扣帳）----------
010426 6610-T6A4-ACCT-RTN.                                              
090701     IF        ( TWA-BR-CODE         NOT =  '026' )               
"                AND ( CTF-AUTO-BR-CODE-1  NOT =  TWA-ACC-BR-CODE-1 )   
"                AND ( CTF-AUTO-BR-CODE-1  NOT =  TWA-ACC-BR-CODE-2 )   
"                AND ( CTF-AUTO-BR-CODE-1  NOT =  TWA-ACC-BR-CODE-3 )   
"                      MOVE  '19693'             TO   T6A4-ACCT-CODE    
"                      MOVE '聯行往來　　　　' TO   T6A4-ACCT-NAME    
010426     ELSE IF   ( CTF-AUTO-ACC-CODE-1  =  '01' )                   
"                      MOVE  '23501'             TO   T6A4-ACCT-CODE    
"                      MOVE '支票存款　　　　' TO   T6A4-ACCT-NAME    
010426     ELSE IF   ( CTF-AUTO-ACC-CODE-1  =  '02' OR '05' OR '07'     
"                                           OR '11' OR '71' OR '73'     
"                                           OR '75' OR '79' )           
"                      MOVE  '23521'             TO   T6A4-ACCT-CODE    
"                      MOVE '活期存款　　　　' TO   T6A4-ACCT-NAME    
010426     ELSE IF   ( CTF-AUTO-ACC-CODE-1  =  '10' OR '06' OR '12'     
"                                           OR '72' OR '74' OR '76'     
090922                                      OR '80' OR '18' )           
"                      MOVE  '23541'             TO   T6A4-ACCT-CODE    
"                      MOVE '活期儲蓄存款　　' TO   T6A4-ACCT-NAME    
010426     ELSE IF   ( CTF-AUTO-ACC-CODE-1  =  '13' )                   
"                      MOVE  '23543'             TO   T6A4-ACCT-CODE    
"                      MOVE '行員活期儲蓄存款' TO   T6A4-ACCT-NAME    
010426     ELSE                                                         
"                      MOVE  SPACES              TO   T6A4-ACCT-CODE    
"                      MOVE  SPACES              TO   T6A4-ACCT-NAME.   
"      6610-EXIT.                                                       
"           EXIT.                                                       
------*---------------國宅融資輸出（LNK125用到)---------------------
       600-OUTPUT-TXN-INFO-B-RTN.
           MOVE        'T613'              TO      MSG-P-OUT-CODE.
860925     MOVE        57                  TO      MSG-P-LENGTH.        
           MOVE        MSG-M-SLIP          TO      MSG-P-OUT-TM-TYPE.
           MOVE        TWA-TM-CODE         TO      MSG-P-SP-TM-CODE.
           MOVE        TWA-ACC-TXN-SEQ-NO  TO      MSG-P-SP-TXN-SEQ-NO.
           MOVE        TWA-TXN-ID-CODE     TO      MSG-P-SP-TXN-ID-CODE.
           PERFORM     690-AMT-TYPE-CHK    THRU    690-EXIT.
           MOVE        SPACE               TO     WS-MSG-CTZN-DATA-SLIP.
           MOVE        W-ACC-NO            TO     W-MSG-CTZN-ACC-NO.
           MOVE        W-LNP-APRV-NO       TO     W-MSG-CTZN-APRV-NO.
           MOVE        LNL-LN-SER-NO       TO     W-MSG-CTZN-LN-SER-NO.
           MOVE        W-BAL               TO     W-MSG-CTZN-AMT.
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      W-MSG-CTZN-150-MARK. 
           MOVE      WS-MSG-CTZN-DATA-SLIP TO     MSG-P-SP-OTHER-DATA.
           CALL        'UCPWRITE'          USING   TWA, MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
 7908      PERFORM   6000-PRNT-JOURNAL-B  THRU    6000-B-EXIT.
       600-B-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       6000-PRNT-JOURNAL-B.
           MOVE        SPACE            TO     WS-MSG-CTZN-DATA-JOURNAL.
           MOVE        TWA-TXN-TIME-HH     TO      WC-MSG-HH.
           MOVE        ':'                 TO      WC-MSG-F1.
           MOVE        TWA-TXN-TIME-MM     TO      WC-MSG-MM.
           MOVE        ':'                 TO      WC-MSG-F2.
           MOVE        TWA-TXN-TIME-SS     TO      WC-MSG-SS.
           MOVE        W-ACC-NO            TO     WC-MSG-CTZN-ACC-NO.
           MOVE        W-LNP-APRV-NO       TO     WC-MSG-CTZN-APRV-NO.
           MOVE        LNL-LN-SER-NO       TO     WC-MSG-CTZN-LN-SER-NO.
           MOVE        W-BAL               TO     WC-MSG-CTZN-AMT.
860925     IF          WK-WARNING-MARK     =    '#'                     
"          THEN                                                         
"                MOVE  '#'                 TO      WC-MSG-CTZN-150-MARK.
           MOVE   WS-MSG-CTZN-DATA-JOURNAL TO     MSG-P-SP-OTHER-DATA.
860925     MOVE        83                  TO      MSG-P-LENGTH.        
           MOVE        MSG-M-JOURNAL       TO      MSG-P-OUT-TM-TYPE.
           MOVE        'T6J2'              TO      MSG-P-OUT-CODE.
           CALL        'UCPWRITE'          USING   TWA, MSG-P-OUT.
           PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.
       6000-B-EXIT.
           EXIT.
------*---------------網銀交易輸出電文處理----------------------------
050614 670-NET-OUTPUT-RTN.                                              
"          MOVE        SPACES               TO   MSG-P-OUT.             
"          MOVE        210                  TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-DISPLAY        TO   MSG-P-OUT-TM-TYPE.     
"          MOVE        'T6A9'               TO   MSG-P-OUT-CODE.        
"          MOVE        '0000'               TO   T6A9-RETURN-CODE       
050614*---------------放款餘額／下次應繳日                            
"          MOVE        LNL-LN-BAL           TO   T6A9-LN-BAL.           
"          MOVE        LNL-RECV-INT-DATE    TO   T6A9-N-RECV-DATE.      
"          IF        ( LNL-RECV-INT-DATE  >  WKS-DUE-DATE )             
"          THEN                                                         
"                      MOVE    WKS-DUE-DATE      TO   T6A9-N-RECV-DATE. 
"     *    ENDIF                                                        
050614*---------------計算〔下次應繳利息〕／設定〔下次應攤本金〕      
"          MOVE        ZEROS                TO   T6A9-N-RECV-INT.       
"          MOVE        ZEROS                TO   T6A9-N-RTRN-AMT.       
"          IF        ( LNL-LN-BAL  >  ZEROS )                           
"          THEN                                                         
"                      PERFORM 671-NEXT-INT-CAL-RTN   THRU  671-EXIT    
"                      MOVE    INT-P-INT-TOTAL   TO   T6A9-N-RECV-INT   
"                      MOVE    ZEROS             TO   WK-WORK-AMT       
"                      PERFORM 672-NEXT-AMT-CAL-RTN   THRU  672-EXIT    
"                      MOVE    WK-WORK-AMT       TO   T6A9-N-RTRN-AMT.  
"     *    ENDIF                                                        
050614*---------------存款餘額                                        
"          IF        ( LPB-PB-BAL  NOT NUMERIC )                        
"                      MOVE    '*'               TO   T6A9-PB-MARK      
"                      MOVE    ZEROS             TO   T6A9-PB-BAL       
"          ELSE IF   ( LPB-PB-BAL  <  ZEROS )                           
"                      MOVE    '-'               TO   T6A9-PB-MARK      
"                      MOVE    LPB-PB-BAL        TO   T6A9-PB-BAL       
"          ELSE                                                         
"                      MOVE    '+'               TO   T6A9-PB-MARK      
"                      MOVE    LPB-PB-BAL        TO   T6A9-PB-BAL.      
      *****ENDIF                                                        
050614*---------------提示訊息                                        
"          MOVE        SPACES               TO   T6A9-WARNING.          
"          PERFORM     673-T6A9-MESSAGE-RTN      THRU  673-EXIT.        
050614*---------------輸出                                            
"          CALL        'UCPWRITE'        USING   TWA, MSG-P-OUT         
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN.                   
"      670-EXIT.                                                        
"          EXIT.                                                        
------*---------------計算〔下次應繳利息〕----------------------------
050614 671-NEXT-INT-CAL-RTN.                                            
"     *---------------計息模組：起始設定                              
"          PERFORM     9171-LN-INT-AREA-CLEAR.                          
"          MOVE        'I'                 TO INT-F-SERV-TYPE-INTC.     
"          MOVE        TWA-TXN-ID-CODE     TO INT-F-TXN-ID-CODE.        
"          MOVE        TWA-TXN-DAT7        TO INT-F-TXN-DATE-R.         
"          MOVE        TWA-BR-CODE         TO INT-F-BR-CODE.            
050614*---------------設定〔攤還方式〕                                
"          IF        ( LNP-RTRN-CODE   =  '9' )                         
"                AND ( LNP-RTRN-INTVL  =  ZEROS )                       
"                      MOVE    '0'               TO   INT-P-RTRN-TYPE   
"          ELSE IF   ( LNP-RTRN-CODE  =  '9' )                          
"                      MOVE    LNP-AFTER-TERM-RTRN-TYPE                 
"                                                TO   INT-P-RTRN-TYPE   
"          ELSE                                                         
"                      MOVE    LNP-RTRN-CODE     TO   INT-P-RTRN-TYPE.  
"     *    ENDIF                                                        
050614*---------------設定：攤還期間／分期攤還額                      
"          IF        ( LNP-RTRN-INTVL  =  '01' )                        
"                      MOVE    01                TO   INT-P-INTERVAL-MM 
"          ELSE IF   ( LNP-RTRN-INTVL  =  '02' )                        
"                      MOVE    06                TO   INT-P-INTERVAL-MM 
"          ELSE                                                         
"                      MOVE    01                TO   INT-P-INTERVAL-MM.
      *****ENDIF                                                        
"          MOVE        LNL-EACH-RTRN-AMT    TO   INT-P-PER-RETURN-AMT.  
050614*---------------設定〔功能碼〕                                  
"          IF        ( INT-P-RTRN-TYPE  =  '2' )                        
"                      MOVE  INT-M-FUN-CODE-24   TO   INT-P-FUN-CODE    
"          ELSE IF   ( LNP-INT-CODE  =  '98' )                          
"                      MOVE  INT-M-FUN-CODE-22   TO   INT-P-FUN-CODE    
"                      MOVE  LNL-CT-INT-CODE     TO   INT-P-CT-RATE-CODE
"                      MOVE  LNL-CT-INCR-DECR-RATE                      
"                                          TO   INT-P-CT-INCR-DECR-RATE 
"          ELSE                                                         
"                      MOVE  INT-M-FUN-CODE-21   TO   INT-P-FUN-CODE.   
      *****ENDIF                                                        
050614*---------------設定：其他介面欄位                              
"          MOVE        SPACES               TO   INT-P-FUN-SUB-CODE.    
"          MOVE        LNL-INT-CODE         TO   INT-P-RATE-CODE.       
"          MOVE        LNP-INT-ADJ-INTVL    TO   INT-P-RATE-ADJ.        
"          MOVE        LNL-INT-RATE         TO   INT-P-RATE.            
"          MOVE    LNL-INT-INCR-DECR-RATE   TO   INT-P-INCR-DECR-RATE.  
"          MOVE        LNP-RECV-INT-TYPE    TO   INT-P-RECV-INT-TYPE.   
"          MOVE        LNP-LN-CODE          TO   INT-P-LN-CODE.         
"          MOVE        LNP-CT-TYPE          TO   INT-P-CT-TYPE.         
"          MOVE        LNP-FINE-TYPE        TO   INT-P-FINE-TYPE.       
"          MOVE        LNL-LN-BAL           TO   INT-P-LOAN-BAL.        
"          MOVE        LNL-RECV-INT-DATE    TO   INT-P-INT-RECV-DATE.   
"          MOVE    LNL-NEXT-INT-ADJ-DATE    TO   INT-P-NX-RATE-ADJ-DATE.
"          MOVE        LNL-L-RECV-INT-DATE  TO   INT-P-INT-BEG-DATE-R.  
"          MOVE        T6A9-N-RECV-DATE     TO   INT-P-INT-END-DATE-R.  
"          MOVE        WKS-DUE-DATE         TO   INT-P-DUE-DATE.        
"          MOVE        LNL-RECV-INT-DAY     TO   INT-P-RECV-BASIC-DD.   
050614     IF        ( LNL-INT-ADJ-DD  =  LNL-1ST-LN-DD )               
"          THEN                                                         
"                      MOVE  LNL-1ST-LN-DD  TO   INT-P-ADJ-BASIC-DD     
"          ELSE                                                         
"                      MOVE  LNL-BOOK-DD    TO   INT-P-ADJ-BASIC-DD.    
      *****ENDIF                                                        
050614     IF        ( LNP-C-ADJ-EACH-6-21 )                            
"                 OR ( LNP-C-ADJ-EACH-12-21 )                           
"                 OR ( LNP-C-ADJ-6-21-12-21 )                           
"          THEN                                                         
"                      MOVE   21            TO   INT-P-ADJ-BASIC-DD     
"                      MOVE  LNL-RECV-INT-DAY                           
"                                           TO   INT-P-RECV-BASIC-DD.   
      *****ENDIF                                                        
050614*---------------資金用途／初貸日／存單開發日                    
"          MOVE        LNP-CAPTL-PURPOSE    TO   INT-P-PURPOSE.         
"          MOVE        LNL-1ST-LN-DATE      TO   INT-P-1ST-DATE.        
090415     MOVE        LNP-CT-ISSUE-DATE    TO   INT-P-CT-ISSUE-DATE.   
050614*---------------設定〔科目〕／約定〔日息〕計算案件科目填００    
"          MOVE        LNL-ACC-CODE         TO   INT-P-ACC-CODE.        
"          IF        ( TBL27-MARK  =  'Y' )                             
"          THEN                                                         
"                      MOVE    ZEROS             TO   INT-P-ACC-CODE.   
      *****ENDIF                                                        
050614*---------------設定〔企消金註記〕                              
"          IF        ( LNM-TAX-ID  =  LOW-VALUE OR SPACES OR ZEROS )    
"          THEN                                                         
"                      MOVE  '9'            TO   INT-P-ENT-TYPE         
"          ELSE                                                         
"                      MOVE  '1'            TO   INT-P-ENT-TYPE.        
      *****ENDIF                                                        
050614*---------------連結：計息模組                                  
"          EXEC        CICS  LINK      PROGRAM ('NACINT7')              
"                                      COMMAREA(INT-GROUP)              
"                                      LENGTH  (INT-LENGTH)             
"                      END-EXEC.                                        
"          PERFORM     9165-INT-SUB-ERR-HDL-RTN.                        
"      671-EXIT.                                                        
"          EXIT.                                                        
------*---------------設定〔下次應攤本金〕----------------------------
050614 672-NEXT-AMT-CAL-RTN.                                            
"     *---------------到期：取現欠餘額                                
"          IF        ( T6A9-N-RECV-DATE  =  WKS-DUE-DATE )              
"                      MOVE    LNL-LN-BAL        TO   WK-WORK-AMT       
050614*---------------〔０〕一般只繳息不攤還方式：填０                
"          ELSE IF   ( INT-P-RTRN-TYPE  =  '0' )                        
"                      MOVE    ZEROS             TO   WK-WORK-AMT       
050614*---------------〔１〕按月平均攤還本金：取分期攤還額            
"          ELSE IF   ( INT-P-RTRN-TYPE  =  '1' )                        
"                      MOVE    LNL-EACH-RTRN-AMT TO   T6A9-N-RTRN-AMT   
050614*---------------〔２〕按月平均攤還本息：分期攤還額－利息        
"          ELSE IF   ( INT-P-RTRN-TYPE  =  '2' )                        
"                      COMPUTE WK-WORK-AMT  =    LNL-EACH-RTRN-AMT      
"                                             -  T6A9-N-RECV-INT        
"          ELSE                                                         
050614*---------------〔其他（※不應發生）〕填０                      
"                      MOVE    ZEROS             TO   WK-WORK-AMT.      
      *****ENDIF                                                        
050614*---------------檢核〔金額〕合理性                              
"          IF        ( WK-WORK-AMT  <  ZEROS )                          
"          THEN                                                         
"                      MOVE    ZEROS             TO   WK-WORK-AMT.      
      *****ENDIF                                                        
"          IF        ( WK-WORK-AMT  >  LNL-LN-BAL )                     
"          THEN                                                         
"                      MOVE    LNL-LN-BAL        TO   WK-WORK-AMT.      
      *****ENDIF                                                        
"      672-EXIT.                                                        
"          EXIT.                                                        
------*---------------網銀繳款〔提示訊息〕設定------------------------
050614 673-T6A9-MESSAGE-RTN.                                            
      *---------------設定：限制清償截止日                            
141124*---------------【１房貸】２年                                  
"          MOVE        LNL-1ST-LN-DATE      TO   WK-WORK-DATE.          
"          IF        ( LNP-LMT-RTN-MARK  =  '1' )                       
"                      ADD     2                 TO   WK-WORK-YY        
141124*---------------【２太陽光電】３年                              
"          ELSE IF   ( LNP-LMT-RTN-MARK  =  '2' )                       
"                      ADD     3                 TO   WK-WORK-YY        
141124*---------------【３客製限制清償】讀ＬＭＴＲ檔設定相關參數      
"          ELSE IF   ( LNP-LMT-RTN-MARK  =  '3' )                       
"                AND ( WLMTR-READ-MARK   =  'Y' )                       
"                      MOVE    WLMTR-END-DATE    TO   WK-WORK-DATE      
"          ELSE                                                         
"                      GO  TO  673-EXIT                                 
"          END-IF.                                                      
050614*-----------------------------------------------------------------
"     *限制清償案件：警語提示                                         
"     *※授管處決議此類客戶限制清償期限內不可在網銀中途攤還           
"     *　在LNL611控管／錯誤訊息M6B5                                 
"     *-----------------------------------------------------------------
"          IF        ( LNP-LMT-RTN-MARK    =  '1' )                     
"                AND ( LNL-ACC-CODE  =  '26' OR '38' OR '25' OR '37' )  
"                AND ( CTF-RECV-TYPE       =  '1'  )                    
"                AND ( TWA-C-TXN-NOT-REVERSE )                          
"                AND ( TWA-TXN-DAT7    NOT >  WK-WORK-DATE )            
"          THEN                                                         
"                      MOVE  WK-PRECLS-WARNING   TO   T6A9-WARNING      
"          END-IF.                                                      
"      673-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
       690-AMT-TYPE-CHK.
           PERFORM     691-CASE-BEG        THRU    691-CASE-END.
       690-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       691-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  691-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  691-CASE-REVERSE.
           GO  TO    691-CASE-ERR.
       691-CASE-NORMAL.
           IF         ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR )         
           THEN
             MOVE      CTF-M-TR            TO      MSG-P-SP-AMT-TYPE
             MOVE      CTF-M-TR            TO      LOG-AMT-TYPE
           ELSE
             MOVE      CTF-M-CASH          TO      MSG-P-SP-AMT-TYPE
             MOVE      CTF-M-CASH          TO      LOG-AMT-TYPE.
           GO  TO      691-CASE-END.
       691-CASE-REVERSE.
           IF          (TWA-C-ID-RCVHI-TR
                    OR  TWA-C-ID-SLIP-TR)
           THEN
             MOVE      CTF-M-TR-REVERSE    TO      MSG-P-SP-AMT-TYPE
             MOVE      CTF-M-TR-REVERSE    TO      LOG-AMT-TYPE
           ELSE
             MOVE      CTF-M-CASH-REVERSE  TO      MSG-P-SP-AMT-TYPE
             MOVE      CTF-M-CASH-REVERSE  TO      LOG-AMT-TYPE.
           GO  TO      691-CASE-END.
       691-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      691-CASE-END.
       691-CASE-END.
           EXIT.
------*----------------印製『轉帳收入、轉帳支出』傳票-----------------
001019 692-TR-SLIP-PROC-RTN.                                            
"     *---------------螢幕顯示提醒列印轉帳支出／收入傳票              
"          MOVE        SPACES               TO   MSG-P-OUT.             
"          MOVE        MSG-M-DISPLAY        TO   MSG-P-OUT-TM-TYPE.     
"          MOVE        'T6K0'               TO   MSG-P-OUT-CODE.        
"          MOVE        90                   TO   MSG-P-LENGTH.          
001019*---------------空行                                            
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
           CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
001019*---------------提醒基本字句１                                  
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
"          MOVE        '注意：'           TO   T6K0-ATTEN.            
"          MOVE        TWA-TXN-ID-CODE      TO   T6K0-TXN-ID.           
"          MOVE        '-'                  TO   T6K0-DASH1.            
"          MOVE        CTF-RECV-TYPE        TO   T6K0-TYPE.             
001019*---------------正常交易提醒                                    
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '1' )                          
"                      MOVE    '放款〔清償〕交易需攤銷協議利差'       
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '3' )                          
"                      MOVE    '放款〔展期〕交易需攤銷協議利差'       
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '5' )                          
"                      MOVE    '放款〔轉催〕交易需攤銷協議利差'       
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '6' )                          
"                      MOVE    '放款〔轉籍〕交易需攤銷協議利差'       
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                      MOVE    '放款〔收回〕交易需攤銷協議利差'       
"                                                TO   T6K0-STATEMENT    
001019*---------------沖正交易提醒                                    
"          ELSE IF   ( CTF-RECV-TYPE  =  '1' )                          
"                      MOVE '放款〔清償〕沖正需同步沖正備呆＆呆帳費用'
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( CTF-RECV-TYPE  =  '3' )                          
"                      MOVE '放款〔展期〕沖正需同步沖正備呆＆呆帳費用'
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( CTF-RECV-TYPE  =  '5' )                          
"                      MOVE '放款〔轉催〕沖正需同步沖正備呆＆呆帳費用'
"                                                TO   T6K0-STATEMENT    
"          ELSE IF   ( CTF-RECV-TYPE  =  '6' )                          
"                      MOVE '放款〔轉籍〕沖正需同步沖正備呆＆呆帳費用'
"                                                TO   T6K0-STATEMENT    
"          ELSE                                                         
"                      MOVE '放款〔收回〕沖正需同步沖正備呆＆呆帳費用'
"                                                TO   T6K0-STATEMENT.   
001019*---------------提醒１                                          
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
001019*---------------提醒２                                          
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
"          MOVE        '　　　請依指示插入轉帳支出／收入傳票，'       
"                                           TO   MSG-P-OUT-MSG.         
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
001019*---------------提醒３                                          
"          MOVE        SPACES               TO   MSG-P-OUT-MSG.         
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"          THEN                                                         
"             MOVE '　　※並登打〔８６１會計明細帳登錄〕交易！'       
"                                           TO   MSG-P-OUT-MSG          
"          ELSE                                                         
"             MOVE '　　※並登打〔８６１１會計明細帳沖正〕交易！'     
010907                                      TO   MSG-P-OUT-MSG.         
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
001019*---------------列印處理                                        
"          PERFORM     6921-SLIP-DB-PRT-RTN      THRU  6921-EXIT.       
"          PERFORM     6922-SLIP-CR-PRT-RTN      THRU  6922-EXIT.       
"      692-EXIT.                                                        
"          EXIT.                                                        
------*---------------列印〔轉帳支出〕傳票（借方）--------------------
001019 6921-SLIP-DB-PRT-RTN.                                            
"     *---------------提醒訊息                                        
"          MOVE        'W062'               TO   MSG-P-OUT-CODE.        
"          PERFORM     9146-LN-ERR-MSG-OUT-RTN   THRU  9146-DUMMY.      
001019*---------------輸出格式設定                                    
"          MOVE        SPACES               TO   MSG-P-OUT.             
"          MOVE        'T116'               TO   MSG-P-OUT-CODE.        
"          MOVE        145                  TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-DOCUMENT-SUPER TO   MSG-P-OUT-TM-TYPE.     
001019*---------------科子目／筆數摘要１                              
"          MOVE        '13590-0301'         TO   T116-ACCT-CODE.        
"          MOVE        '備抵呆帳'         TO   T116-ACCT-NAME.        
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '1' )                          
"                  MOVE '放款清償攤銷協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '3' )                          
"                  MOVE '放款展期攤銷協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '5' )                          
"                  MOVE '放款轉催攤銷協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '6' )                          
"                  MOVE '放款轉籍攤銷協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                  MOVE '放款收回攤銷協議利差借方１件' TO T116-MEMO1  
001019*---------------沖正交易提醒                                    
"          ELSE IF   ( CTF-RECV-TYPE  =  '1' )                          
"                  MOVE '放款清償沖正協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( CTF-RECV-TYPE  =  '3' )                          
"                  MOVE '放款展期沖正協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( CTF-RECV-TYPE  =  '5' )                          
"                  MOVE '放款轉催沖正協議利差借方１件' TO T116-MEMO1  
"          ELSE IF   ( CTF-RECV-TYPE  =  '6' )                          
"                  MOVE '放款轉籍沖正協議利差借方１件' TO T116-MEMO1  
"          ELSE                                                         
"                  MOVE '收回清償沖正協議利差借方１件' TO T116-MEMO1  
"          END-IF.                                                      
001019*---------------日期／帳號／金額                                
"          MOVE        TWA-TXN-DAT7         TO   T116-TXN-DATE.         
"          MOVE        AGDI-ACC-NO          TO   T116-ACC-NO.           
"          MOVE        '-'                  TO   T116-DASH1.            
"          MOVE        AGDI-APRV-NO         TO   T116-APRV-NO.          
"          MOVE        WAGDI-DIFF-BAL       TO   T116-TXN-AMT.          
001019*---------------摘要２／摘要３                                  
"          MOVE        SPACES               TO   T116-MEMO2.            
"          MOVE        '時間：'           TO   T116-M2-TIME-A.        
"          MOVE        TWA-TXN-TIME         TO   T116-M2-TIME.          
"          MOVE        '櫃員：'           TO   T116-M2-TELLER-A.      
"          MOVE        TWA-TELLER-CODE      TO   T116-M2-TELLER.        
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"          THEN                                                         
"                  MOVE '需登打８６１交易　'   TO   T116-MEMO3        
"          ELSE                                                         
"                  MOVE '需登打８６１１交易'   TO   T116-MEMO3.       
001019*---------------輸出                                            
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
"      6921-EXIT.                                                       
"           EXIT.                                                       
------*---------------列印〔轉帳收入〕傳票（貸方）--------------------
001019 6922-SLIP-CR-PRT-RTN.                                            
"     *---------------提醒訊息                                        
"          MOVE        'W063'               TO   MSG-P-OUT-CODE.        
"          PERFORM     9146-LN-ERR-MSG-OUT-RTN   THRU  9146-DUMMY.      
001019*---------------輸出格式設定                                    
"          MOVE        SPACES               TO   MSG-P-OUT.             
"          MOVE        'T116'               TO   MSG-P-OUT-CODE.        
"          MOVE        145                  TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-DOCUMENT-SUPER TO   MSG-P-OUT-TM-TYPE.     
001019*---------------科子目／筆數摘要１                              
"          MOVE        '51501-0200'         TO   T116-ACCT-CODE.        
"          MOVE        '呆帳費用'         TO   T116-ACCT-NAME.        
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '1' )                          
"                      MOVE    '放款清償攤銷協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '3' )                          
"                      MOVE    '放款展期攤銷協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '5' )                          
"                      MOVE    '放款轉催攤銷協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                AND ( CTF-RECV-TYPE  =  '6' )                          
"                      MOVE    '放款轉籍攤銷協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( NOT   TWA-C-TXN-REVERSE )                        
"                      MOVE    '放款收回攤銷協議利差貸方１件'         
"                                                TO   T116-MEMO1        
001019*---------------沖正交易提醒                                    
"          ELSE IF   ( CTF-RECV-TYPE  =  '1' )                          
"                      MOVE    '放款清償沖正協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( CTF-RECV-TYPE  =  '3' )                          
"                      MOVE    '放款展期沖正協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( CTF-RECV-TYPE  =  '5' )                          
"                      MOVE    '放款轉催沖正協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE IF   ( CTF-RECV-TYPE  =  '6' )                          
"                      MOVE    '放款轉籍沖正協議利差貸方１件'         
"                                                TO   T116-MEMO1        
"          ELSE                                                         
"                      MOVE    '收回清償沖正協議利差貸方１件'         
"                                                TO   T116-MEMO1.       
001019*---------------日期／帳號／金額                                
"          MOVE        TWA-TXN-DAT7         TO   T116-TXN-DATE.         
"          MOVE        AGDI-ACC-NO          TO   T116-ACC-NO.           
"          MOVE        '-'                  TO   T116-DASH1.            
"          MOVE        AGDI-APRV-NO         TO   T116-APRV-NO.          
"          MOVE        WAGDI-DIFF-BAL       TO   T116-TXN-AMT.          
001019*---------------摘要２／摘要３                                  
"          MOVE        SPACES               TO   T116-MEMO2.            
"          MOVE        '時間：'           TO   T116-M2-TIME-A.        
"          MOVE        TWA-TXN-TIME         TO   T116-M2-TIME.          
"          MOVE        '櫃員：'           TO   T116-M2-TELLER-A.      
"          MOVE        TWA-TELLER-CODE      TO   T116-M2-TELLER.        
"          IF        ( NOT   TWA-C-TXN-REVERSE )                        
"          THEN                                                         
"                      MOVE '需登打８６１交易　'    TO  T116-MEMO3    
"          ELSE                                                         
"                      MOVE '需登打８６１１交易'    TO  T116-MEMO3.   
001019*---------------輸出                                            
"          CALL        'UCPWRITE'        USING   TWA  MSG-P-OUT.        
"          PERFORM     9147-LN-UCP-WRITE-ERR-HDL-RTN  THRU  9147-DUMMY. 
"      6922-EXIT.                                                       
"           EXIT.                                                       
------*---------------ＢＩＦＭ＆會計帳處理----------------------------
       700-BIF-PREPARE-RTN.
      *-----------------原始有效利率帳務調整作業                      
991213     IF        ( ( TWA-C-TXN-NOT-REVERSE  )                       
991203     AND         ( SAVE-ADJ-AMT  NOT =  ZEROS ) )                 
           OR        ( ( TWA-C-TXN-REVERSE      )                       
991203     AND         ( REV-SAVE-ADJ-AMT  IS NUMERIC   )               
991203     AND         ( REV-SAVE-ADJ-AMT  NOT =  ZEROS ) )             
"          THEN                                                         
"            PERFORM     7300-UP-AC-CRF-RTN   THRU  7300-EXIT.          
      *****ENDIF                                                        
      *---------------設定：ＢＩＦＭ介面欄位                          
890309     MOVE        ZEROS               TO   BIF-P-AREA.             
           PERFORM     9175-LN-BIF-CLEAR-RTN.                           
           MOVE        BIF-M-UPDATE        TO   BIF-P-FUN-CODE.
090701     MOVE        LNM-AGENT-BR        TO   BIF-P-BR-CODE (1).      
"          MOVE        LNM-AGENT-BR        TO   BIF-P-BR-CODE (2).      
           MOVE        LNL-DEP-CODE        TO   BIF-P-DEP-CODE (1).
           MOVE        LNL-DEP-CODE        TO   BIF-P-DEP-CODE (2).
           MOVE        LNL-CURRENCY-CODE   TO   BIF-P-CURRENCY-CODE (1).
           MOVE        LNL-CURRENCY-CODE   TO   BIF-P-CURRENCY-CODE (2).
           MOVE        LNL-ACC-CODE        TO   BIF-P-ACC-CODE (1).
           MOVE        LNL-ACC-CODE        TO   BIF-P-ACC-CODE (2).
           MOVE        CLNP-LN-CODE        TO   BIF-P-CHAR-CODE (1).    
           MOVE        CLNP-LN-CODE        TO   BIF-P-CHAR-CODE (2).    
      *---------------國宅：子目轉換為社區別                          
           IF          CTF-ACC-BUSINS-CODE  =  69
                  AND  LNL-C-LN-RC-FOR-OTHER
           THEN
             PERFORM   9210-CONVER-APRV-RTN THRU   9210-EXIT            
             MOVE      W-LNP-APRV-PLAN-NO  TO      BIF-P-CHAR-CODE (1)
             MOVE      W-LNP-APRV-PLAN-NO  TO      BIF-P-CHAR-CODE (2).
      *****ENDIF                                                        
      *----------------現金／轉帳                                     
           IF          (TWA-C-ID-RCVHI-TR
                    OR  TWA-C-ID-SLIP-TR)
           THEN
             MOVE      BIF-M-TR-CR         TO   BIF-P-FLD-ID (1)
           ELSE
             MOVE      BIF-M-CASH-CR       TO   BIF-P-FLD-ID (1).
      *****ENDIF                                                        
      *---------------件數異動                                        
           MOVE        BIF-M-ADD           TO   BIF-P-FLD-FUN (1).
           MOVE        BIF-M-CLOSE-NO      TO   BIF-P-FLD-ID (2).
           MOVE        BIF-M-ADD           TO   BIF-P-FLD-FUN (2).
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
             MOVE      CTF-RECV-AMT        TO   BIF-P-AMT (1)
             MOVE      1                   TO   BIF-P-TXN-NO (1)
           ELSE
             COMPUTE   BIF-P-AMT (1) = -1 * CTF-RECV-AMT
             MOVE      -1                  TO   BIF-P-TXN-NO (1).
      *****ENDIF                                                        
           PERFORM     710-CASE-BEG        THRU 710-CASE-END.
           PERFORM     720-UPDATE-INT-ACC  THRU 720-EXIT.
      *---------------連動處理ＢＩＦＭ／ＢＩＦＴ                      
050614     IF        ( TWA-BR-CODE  =  '026' )                          
"          THEN                                                         
090701                 MOVE  LNM-AGENT-BR        TO   TWA-BR-CODE       
"                      EXEC  CICS LINK PROGRAM ('UCPBIFUA')             
"                                      COMMAREA(BIF-P-AREA)             
"                                      LENGTH  (BIF-LENGTH)             
"                      END-EXEC                                         
"                      MOVE  '026'               TO   TWA-BR-CODE       
"          ELSE                                                         
                       EXEC  CICS LINK PROGRAM ('UCPBIFUA')             
890309                                 COMMAREA(BIF-P-AREA)             
                                       LENGTH  (BIF-LENGTH)             
                       END-EXEC.
      *****ENDIF                                                        
           PERFORM     9162-LN-BIF-ERR-HDL-RTN.
      *---------------處理：ＡＣＣＲＦ會計明細分類帳                  
871031     PERFORM     730-UP-AC-CRF-RTN   THRU  730-EXIT.              
      *---------------處理：ＴＩＦ櫃員現金檔                          
           IF        ( TWA-C-ID-RCVHI-CH OR TWA-C-ID-RCVHI-MX           
                   OR  TWA-C-ID-SLIP-MX  OR TWA-C-ID-SLIP-CH )          
           THEN
             PERFORM   740-TIF-DBCR-CHANG   THRU   740-CASE-END
           ELSE
             GO   TO   700-EXIT.
           MOVE        ZEROS                   TO     TEL-REC.
           MOVE    TWA-TXN-ID-CODE-1           TO     TEL-TXN-CODE.
           IF      TWA-C-TXN-REVERSE
              MOVE      2                      TO     TEL-DBCR-CODE
              MOVE      'EC'                   TO     TEL-REV-CODE
           ELSE
              MOVE      1                      TO     TEL-DBCR-CODE
              MOVE     '  '                    TO     TEL-REV-CODE.
           MOVE        TWA-ACC-TXN-SEQ-NO      TO     TEL-TXN-SEQ-NO.
860925     IF          WK-WARNING-MARK     =     '#'                    
"          THEN                                                         
"             MOVE    '$$$'                    TO     TEL-MSG.          
           PERFORM     9104-TEL-WRITE-RTN.
       700-EXIT.
           EXIT.
------*-----------------------------------------------------------------
960521 700A-LNBAD-BIF-PREPARE-RTN.     COPY LNK70A.                     
"      700A-EXIT.  EXIT.                                                
"      700A1-LNBAD-BIF-PREPARE-RTN.    COPY LNK70A1.                    
"      700A1-EXIT. EXIT.                                                
"      700B-LNBAD-BIF-PREPARE-RTN.     COPY LNK70B.                     
"      700B-EXIT.  EXIT.                                                
"      700B1-LNBAD-BIF-PREPARE-RTN.    COPY LNK70B1.                    
"      700B1-EXIT. EXIT.                                                
961226 700C-SCH-TRUST-BIF-RTN.         COPY LNK70C.                     
       700C-EXIT.  EXIT.                                                
961226 700C1-LNBAD-BIF-PREPARE-RTN.    COPY LNK70C1.                    
       700C1-EXIT. EXIT.                                                
       700C2-LNBAD-BIF-PREPARE-RTN.    COPY LNK70C2.                    
"      700C2-EXIT.  EXIT.                                               
961226 700D-SCH-TRUST-BIF-RTN.         COPY LNK70D.                     
       700D-EXIT.  EXIT.                                                
"      700D1-LNBAD-BIF-PREPARE-RTN.    COPY LNK70D1.                    
"      700D1-EXIT. EXIT.                                                
"      700D2-LNBAD-BIF-PREPARE-RTN.    COPY LNK70D2.                    
"      700D2-EXIT. EXIT.                                                
0606XX 700G-BAD69-BIF-PREPARE-RTN.    COPY LNK70G.                      
"      700G-EXIT.  EXIT.                                                
0606XX 700G1-BAD69-BIF-PREPARE-RTN.    COPY LNK70G1.                    
"      700G1-EXIT.  EXIT.                                               
0606XX 700H-BAD69-BIF-PREPARE-RTN.    COPY LNK70H.                      
"      700H-EXIT.  EXIT.                                                
0606XX 700H1-BAD69-BIF-PREPARE-RTN.    COPY LNK70H1.                    
"      700H1-EXIT.  EXIT.                                               
------*---------------連動會計：款折溢價調整科子目--------------------
991203 7300-UP-AC-CRF-RTN.                                              
      *---------------排除：非放款科目                                
"          IF        ( LNL-ACC-CODE  < 23 )                             
"          OR        ( LNL-ACC-CODE  > 38 )                             
"                      GO  TO   7300-EXIT.                              
      *****ENDIF                                                        
"          MOVE        LNL-ACC-CODE        TO   ACRF-CHAR-NO.           
090701     MOVE        LNM-AGENT-BR      TO     ACRF-BR-CODE.           
000131     MOVE    86                    TO     ACRF-ACC-CODE.          
"          IF      TWA-C-TXN-NOT-REVERSE                                
"              MOVE  TWA-TM-ID-NO        TO     WS-REV-TM-NO            
"              MOVE  TWA-ACC-TXN-SEQ-NO  TO     WS-REV-SEQ-NO           
"              MOVE  WS-REV-NO           TO     ACRF-TXN-SEQ-NO         
"          ELSE                                                         
"              MOVE  WK-CTF-TXN-SER-NO   TO     ACRF-TXN-SEQ-NO.        
      *****ENDIF                                                        
"          MOVE    CTF-ACC-NO            TO     ACRF-ACC-ID-NO.         
"          MOVE    1                     TO     ACRF-CSTR-CODE.         
991203     IF      TWA-C-TXN-NOT-REVERSE                                
"          IF          ( SAVE-ADJ-AMT  >  ZEROS )                       
"          THEN                                                         
"            MOVE    1                     TO     ACRF-DBCR-CODE        
"            MOVE    SAVE-ADJ-AMT          TO     ACRF-TXN-AMT          
"          ELSE                                                         
"            COMPUTE ACRF-TXN-AMT          =      (-1) * SAVE-ADJ-AMT   
"            MOVE    2                     TO     ACRF-DBCR-CODE.       
      *****ENDIF                                                        
991203     IF      TWA-C-TXN-REVERSE                                    
"          IF          ( REV-SAVE-ADJ-AMT  >  ZEROS )                   
"          THEN                                                         
"            MOVE    1                     TO     ACRF-DBCR-CODE        
"            MOVE    REV-SAVE-ADJ-AMT      TO     ACRF-TXN-AMT          
"          ELSE                                                         
"            COMPUTE ACRF-TXN-AMT          =  (-1) * REV-SAVE-ADJ-AMT   
"            MOVE    2                     TO     ACRF-DBCR-CODE.       
"          MOVE   '　　　　　'     TO     ACRF-CH-NAME.               
      *****ENDIF                                                        
000131     MOVE   LNP-APRV-NO        TO     WK-APRV-NO.                 
000131     MOVE   LNL-LN-SER-NO      TO     WK-LN-SER-NO.               
000131     MOVE   WK-LN-ID-NO        TO     ACRF-LN-ID.                 
000131     IF  ( WK-COST-MARK  =  'Y' )                                 
"          THEN                                                         
"            MOVE   1                  TO     ACRF-TYPE                 
"          ELSE                                                         
"            MOVE   2                  TO     ACRF-TYPE.                
000131     MOVE   2                    TO     ACRF-UP-CODE.             
"          MOVE   CTF-INT-BGN-DATE-N-R TO     ACRF-INT-BEG-DATE.        
000131     MOVE   CTF-INT-END-DATE-N-R TO     ACRF-INT-END-DATE.        
           PERFORM 9630-LINK-ACLACR-RTN THRU  9630-EXIT.                
"          PERFORM 964-RETURN-HAND-RTN THRU  964-EXIT.                  
"      7300-EXIT.                                                       
991203     EXIT.                                                        
------*---------------處理〔利息／違約金／手續費〕會計明細帳----------
871031 730-UP-AC-CRF-RTN.                                               
      *---------------排除：代放款                                    
880210     IF          LNL-ACC-CODE     =  (43 OR 53 OR 44 OR 54)       
                       GO  TO   730-EXIT.                               
      *****ENDIF                                                        
090701     MOVE        LNM-AGENT-BR         TO   ACRF-BR-CODE.          
           MOVE        BIF-P-ACC-CODE(3)    TO   ACRF-ACC-CODE.         
           MOVE        BIF-P-CHAR-CODE(3)   TO   ACRF-CHAR-NO.          
970429*---------------「就學貸款」會計獨立：代碼另設                  
"          IF        ( LNP-LN-CODE  =  '55' )                           
"          THEN                                                         
"                      MOVE    55           TO   ACRF-CHAR-NO.          
      *****ENDIF                                                        
000225*---------------營業部〔市政貸款〕會計獨立                      
"          IF        ( F-101-MARK = 'Y' )                               
           THEN                                                         
"                      MOVE    '1'          TO   ACRF-101-CODE          
           ELSE                                                         
"                      MOVE    '0'          TO   ACRF-101-CODE.         
      *****ENDIF                                                        
      *---------------設定：交易序號                                  
           IF        ( TWA-C-TXN-NOT-REVERSE )                          
           THEN                                                         
                       MOVE  TWA-TM-ID-NO        TO   WS-REV-TM-NO      
                       MOVE  TWA-ACC-TXN-SEQ-NO  TO   WS-REV-SEQ-NO     
                       MOVE  WS-REV-NO           TO   ACRF-TXN-SEQ-NO   
           ELSE                                                         
940517                 MOVE  WK-CTF-TXN-SER-NO   TO   ACRF-TXN-SEQ-NO.  
      *****ENDIF                                                        
      *---------------設定：放款帳號                                  
           MOVE        CTF-ACC-NO           TO   ACRF-ACC-ID-NO.        
      *---------------設定：現金／轉帳                                
           IF        ( BIF-P-FLD-ID(3)  =  08 )                         
           THEN                                                         
                       MOVE    0            TO   ACRF-CSTR-CODE         
           ELSE                                                         
                       MOVE    1            TO   ACRF-CSTR-CODE.        
      *****ENDIF                                                        
      *---------------設定：聯行註記／借貸別（貸方）                  
090701     IF        ( LNM-AGENT-BR  NOT =  TWA-BR-CODE )               
           THEN                                                         
                       MOVE    1            TO     ACRF-CSTR-CODE.      
      *****ENDIF                                                        
           MOVE        2                    TO     ACRF-DBCR-CODE.      
      *---------------設定：摘要                                      
050614     IF        ( TWA-BR-CODE  =  '026' )                          
"                      MOVE   '網銀收息'       TO   ACRF-CH-NAME      
090701     ELSE IF   ( TWA-BR-CODE  NOT =  LNM-AGENT-BR )               
                       MOVE   '聯行代收息'     TO   ACRF-CH-NAME      
           ELSE IF   ( TWA-C-ID-RCVHI-TR  OR  TWA-C-ID-SLIP-TR )        
                       MOVE   '轉帳收息　'     TO   ACRF-CH-NAME      
           ELSE                                                         
                       MOVE   '現金收息　'     TO   ACRF-CH-NAME.     
      *****ENDIF                                                        
090522*-----------------------------------------------------------------
"     *展延利息（LBINT檔）屬「19:受新冠肺炎疫情影響展延」案件，   
"     *依財務會計處陳鳳明副處長意見，另設應收利息子目，               
"     *-----------------------------------------------------------------
"          IF        ( CTF-INT-AMT  NOT =  ZEROS )                      
"          THEN                                                         
"            IF      ( WK-LABOR-DELAY-INT  NOT =  ZEROS )               
"                AND ( LBI-CUST-TYPE           =  '19' )                
"            THEN                                                       
"                      PERFORM 732-ACLACR-LBI-RTN     THRU  732-EXIT    
"            ELSE                                                       
                       MOVE    CTF-INT-AMT       TO   ACRF-TXN-AMT      
                       PERFORM 963-LINK-ACLACR-RTN    THRU  963-EXIT    
                       PERFORM 964-RETURN-HAND-RTN    THRU  964-EXIT.   
      *****ENDIF                                                        
      *---------------逾期息／違約金                                  
           IF      LNL-C-SAVE-ACC-CODE                                  
           THEN                                                         
                   MOVE    94            TO     ACRF-ACC-CODE           
           ELSE                                                         
                   MOVE    84            TO     ACRF-ACC-CODE.          
      *****ENDIF                                                        
           IF      CTF-FINE     NOT      =      ZEROS                   
                   MOVE    CTF-FINE      TO     ACRF-TXN-AMT            
                   PERFORM 963-LINK-ACLACR-RTN THRU  963-EXIT           
                   PERFORM 964-RETURN-HAND-RTN THRU  964-EXIT.          
      *****ENDIF                                                        
890309 730-EXIT.                                                        
           EXIT.                                                        
------*---------------展延利息之應收利息另設子目----------------------
090522 732-ACLACR-LBI-RTN.                                              
"          IF        ( CTF-INT-AMT  >  WK-LABOR-DELAY-INT )             
"          THEN                                                         
"     *---------------一般應收利息（８３／９３）                      
"                      COMPUTE ACRF-TXN-AMT =    CTF-INT-AMT            
"                                             -  WK-LABOR-DELAY-INT     
"                      PERFORM 963-LINK-ACLACR-RTN    THRU  963-EXIT    
"                      PERFORM 964-RETURN-HAND-RTN    THRU  964-EXIT    
"     *---------------展延應收利息（科目代碼＝８６）                  
"                      MOVE    '86'              TO   ACRF-ACC-CODE     
"                      MOVE  WK-LABOR-DELAY-INT  TO   ACRF-TXN-AMT      
"                      PERFORM 963-LINK-ACLACR-RTN    THRU  963-EXIT    
"                      PERFORM 964-RETURN-HAND-RTN    THRU  964-EXIT    
"          ELSE                                                         
"                      MOVE    '86'              TO   ACRF-ACC-CODE     
"                      MOVE    CTF-INT-AMT       TO   ACRF-TXN-AMT      
"                      PERFORM 963-LINK-ACLACR-RTN    THRU  963-EXIT    
"                      PERFORM 964-RETURN-HAND-RTN    THRU  964-EXIT.   
      *****ENDIF                                                        
"      732-EXIT.                                                        
           EXIT.                                                        
------*---------------櫃員現金檔處理----------------------------------
       740-TIF-DBCR-CHANG.
      *---------------借方發生數                                      
           MOVE        TWA-TXN-STATUS       TO   TIF-P-TXN-NO.          
           MOVE        'DB1'                TO   TIF-P-FUN-CODE.        
           COMPUTE     TIF-P-TXN-AMT = CTF-W-LOG-AMT * TWA-TXN-STATUS.  
           PERFORM     977-TIF-HDL-RTN.                                 
920916******************************************************************
"     *現金大額付款（超過100萬由大出納收）：                        
"     *．現金交易：６７１、６７２                                     
"     *．混合交易：６７４、６７５（需扣除交換票據金額）               
"     ******************************************************************
"          IF        ( TWA-C-ID-RCVHI-CH OR TWA-C-ID-SLIP-CH )          
"          THEN                                                         
"            IF      ( CTF-W-LOG-AMT  >  1000000 )                      
"            THEN                                                       
"                      COMPUTE TIF-P-TXN-AMT =   CTF-W-LOG-AMT          
"                                              * TWA-TXN-STATUS         
"                      MOVE  TWA-TXN-STATUS      TO   TIF-P-TXN-NO      
"                      MOVE  'CR4'               TO   TIF-P-FUN-CODE    
"                      PERFORM  977-TIF-HDL-RTN.                        
      *****ENDIF                                                        
920916     IF        ( TWA-C-ID-RCVHI-MX OR TWA-C-ID-SLIP-MX )          
"          THEN                                                         
"            IF      ( CTF-RECV-CASH-AMT  >  1000000 )                  
"            THEN                                                       
"                      COMPUTE TIF-P-TXN-AMT =   CTF-RECV-CASH-AMT      
"                                              * TWA-TXN-STATUS         
"                      MOVE  TWA-TXN-STATUS      TO   TIF-P-TXN-NO      
"                      MOVE  'CR4'               TO   TIF-P-FUN-CODE    
"                      PERFORM 977-TIF-HDL-RTN.                         
      *****ENDIF                                                        
920916*---------------票據發生數（交大出納）：混合交易６７４、６７５  
"          IF        ( TWA-C-ID-RCVHI-MX OR TWA-C-ID-SLIP-MX )          
"          THEN                                                         
"                      COMPUTE TIF-P-TXN-AMT =   CTF-W-LOG-AMT          
"                                              - CTF-RECV-CASH-AMT      
"            IF      ( TIF-P-TXN-AMT  >  ZEROS )                        
"            THEN                                                       
"                      COMPUTE TIF-P-TXN-AMT =   TIF-P-TXN-AMT          
"                                              * TWA-TXN-STATUS         
"                      MOVE  TWA-TXN-STATUS      TO   TIF-P-TXN-NO      
"                      MOVE  'CR3'               TO   TIF-P-FUN-CODE    
920916                 PERFORM  977-TIF-HDL-RTN.                        
      *****ENDIF                                                        
       740-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       710-CASE-BEG.
           IF        (TWA-C-TXN-NOT-REVERSE)  GO TO  710-CASE-NORMAL.
           IF        (TWA-C-TXN-REVERSE)      GO TO  710-CASE-REVERSE.
           GO  TO    710-CASE-ERR.
       710-CASE-NORMAL.
           IF         ( LNL-C-RTRN OR LNL-C-TR OR LNL-C-TR-OVER         
                    OR  LNL-C-MVOUT )                                   
           THEN
             PERFORM   711-UPDATE-RTRN-NO  THRU    711-EXIT.
      *****ENDIF                                                        
           GO  TO      710-CASE-END.
       710-CASE-REVERSE.
           IF         ( W-LNL-C-RTRN OR W-LNL-C-TR OR W-LNL-C-TR-OVER   
                    OR  W-LNL-C-MVOUT )                                 
           THEN
             PERFORM   711-UPDATE-RTRN-NO  THRU    711-EXIT.
      *****ENDIF                                                        
           GO  TO      710-CASE-END.
       710-CASE-ERR.
           MOVE        'D018'              TO      MSG-P-OUT-CODE.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.
           GO  TO      710-CASE-END.
       710-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       711-UPDATE-RTRN-NO.
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
             MOVE      0                   TO   BIF-P-AMT (2)
             MOVE      1                   TO   BIF-P-TXN-NO (2)
           ELSE
             MOVE      0                   TO   BIF-P-AMT (2)
             MOVE      -1                  TO   BIF-P-TXN-NO (2).
      *    ENDIF
       711-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       720-UPDATE-INT-ACC.
           PERFORM     9149-INT-ACC-TR-RTN.
090701     MOVE        LNM-AGENT-BR         TO   BIF-P-BR-CODE(3).      
"          MOVE        LNM-AGENT-BR         TO   BIF-P-BR-CODE(4).      
           MOVE        LNL-DEP-CODE        TO      BIF-P-DEP-CODE (3).
           MOVE        LNL-DEP-CODE        TO      BIF-P-DEP-CODE (4).
           MOVE        LNL-CURRENCY-CODE   TO  BIF-P-CURRENCY-CODE (3).
           MOVE        LNL-CURRENCY-CODE   TO  BIF-P-CURRENCY-CODE (4).
           MOVE        LNL-ACC-CODE        TO      BIF-P-CHAR-CODE (3).
           MOVE        LNL-ACC-CODE        TO      BIF-P-CHAR-CODE (4).
           IF          (CTF-ACC-BUSINS-CODE  =  69)
                  AND  (LNL-C-LN-RC-FOR-OTHER)
           THEN
840913       PERFORM   9210-CONVER-APRV-RTN THRU   9210-EXIT            
             MOVE      W-LNP-APRV-PLAN-NO  TO      BIF-P-CHAR-CODE (3)
             MOVE      W-LNP-APRV-PLAN-NO  TO      BIF-P-CHAR-CODE (4).
      *****ENDIF                                                        
           PERFORM     722-CASE-BEGIN      THRU    722-CASE-END.
       720-EXIT.
           EXIT.
------*-----------------------------------------------------------------
       722-CASE-BEGIN.
           IF       TWA-C-TXN-NOT-REVERSE      GO TO 722-CASE-1.
           IF       TWA-C-TXN-REVERSE          GO TO 722-CASE--1.
       722-CASE-1.
           IF        ( TWA-C-ID-RCVHI-TR  OR TWA-C-ID-SLIP-TR )         
           THEN
             MOVE      BIF-M-TR-CR         TO      BIF-P-FLD-ID (3)
                                                   BIF-P-FLD-ID (4)
           ELSE
             MOVE      BIF-M-CASH-CR       TO      BIF-P-FLD-ID (3)
                                                   BIF-P-FLD-ID (4).
      *****ENDIF                                                        
           IF   NOT  ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR            
                    OR TWA-C-ID-RCVHI-CH OR TWA-C-ID-RCVHI-MX           
                    OR TWA-C-ID-SLIP-CH  OR TWA-C-ID-SLIP-MX )          
           THEN
             MOVE      'D022'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
      *****ENDIF                                                        
           MOVE        CTF-INT-AMT         TO      BIF-P-AMT (3).
           MOVE        CTF-FINE            TO      BIF-P-AMT (4).
           IF          CTF-INT-AMT         =   0
           THEN
             MOVE      0                   TO      BIF-P-TXN-NO (3)
           ELSE
             MOVE      1                   TO      BIF-P-TXN-NO (3).
      *****ENDIF                                                        
           IF          CTF-FINE            =   0
           THEN
             MOVE      0                   TO      BIF-P-TXN-NO (4)
           ELSE
             MOVE      1                   TO      BIF-P-TXN-NO (4).
      *****ENDIF                                                        
           MOVE        BIF-M-ADD           TO      BIF-P-FLD-FUN (3).
           MOVE        BIF-M-ADD           TO      BIF-P-FLD-FUN (4).
           GO          TO                  722-CASE-END.
       722-CASE--1.
           IF        ( TWA-C-ID-RCVHI-TR  OR TWA-C-ID-SLIP-TR )         
           THEN
             MOVE      BIF-M-TR-CR         TO      BIF-P-FLD-ID (3)
                                                   BIF-P-FLD-ID (4)
           ELSE
             MOVE      BIF-M-CASH-CR       TO      BIF-P-FLD-ID (3)
                                                   BIF-P-FLD-ID (4).
      *    ENDIF
           IF   NOT  ( TWA-C-ID-RCVHI-TR OR TWA-C-ID-SLIP-TR            
                    OR TWA-C-ID-RCVHI-CH OR TWA-C-ID-RCVHI-MX           
                    OR TWA-C-ID-SLIP-CH  OR TWA-C-ID-SLIP-MX )          
           THEN
             MOVE      'D022'              TO      MSG-P-OUT-CODE
             PERFORM   9146-LN-ERR-MSG-OUT-RTN.
      *    ENDIF
           IF          CTF-INT-AMT         =   0
           THEN
             MOVE      0          TO   BIF-P-TXN-NO(3) BIF-P-AMT(3)     
           ELSE
             MOVE     -1                   TO      BIF-P-TXN-NO (3)
             COMPUTE   BIF-P-AMT (3)       =       CTF-INT-AMT *  -1.
      *    ENDIF
           IF          CTF-FINE            =   0
           THEN
             MOVE      0          TO   BIF-P-TXN-NO(4) BIF-P-AMT(4)     
           ELSE
             MOVE     -1                   TO      BIF-P-TXN-NO (4)
             COMPUTE   BIF-P-AMT (4)       =       CTF-FINE    *  -1.
      *    ENDIF
           MOVE        BIF-M-ADD           TO      BIF-P-FLD-FUN (3).
           MOVE        BIF-M-ADD           TO      BIF-P-FLD-FUN (4).
       722-CASE-END.
           EXIT.
------*-----------------------------------------------------------------
       700-BIF-PREPARE-B-RTN.
890309     MOVE        ZEROS               TO      BIF-P-AREA.          
           MOVE        BIF-M-UPDATE        TO      BIF-P-FUN-CODE.
090701     MOVE        LNM-AGENT-BR        TO      BIF-P-BR-CODE (1).   
           MOVE        LNL-DEP-CODE        TO      BIF-P-DEP-CODE (1).
           MOVE        LNL-CURRENCY-CODE   TO  BIF-P-CURRENCY-CODE (1).
           MOVE        LNL-ACC-CODE        TO      BIF-P-ACC-CODE (1).
           MOVE        LNP-LN-CODE         TO      BIF-P-CHAR-CODE (1).
           IF          (TWA-C-ID-RCVHI-TR
                    OR  TWA-C-ID-SLIP-TR)
           THEN
             MOVE      BIF-M-TR-CR         TO   BIF-P-FLD-ID (1)
           ELSE
             MOVE      BIF-M-CASH-CR       TO   BIF-P-FLD-ID (1).
      *****ENDIF                                                        
           MOVE        BIF-M-ADD           TO   BIF-P-FLD-FUN (1).
           IF          (TWA-C-TXN-NOT-REVERSE)
           THEN
             MOVE      W-BAL               TO   BIF-P-AMT (1)
             MOVE      1                   TO   BIF-P-TXN-NO (1)
           ELSE
             COMPUTE   BIF-P-AMT (1) = -1 * W-BAL
             MOVE      -1                  TO   BIF-P-TXN-NO (1).
      *****ENDIF                                                        
           EXEC        CICS             LINK    PROGRAM ('UCPBIFUA')
"                      COMMAREA (BIF-P-AREA)    LENGTH (BIF-LENGTH)     
                       END-EXEC.
           PERFORM     9162-LN-BIF-ERR-HDL-RTN.
       700-B-EXIT.
           EXIT.
------*---------------連動ＰＢ扣款處理--------------------------------
931216 750-UP-PB-RTN.                                                   
           IF          TWA-C-TXN-NOT-REVERSE                            
           THEN                                                         
             MOVE      1                   TO    LPB-FUN-CODE           
           ELSE                                                         
             MOVE      2                   TO    LPB-FUN-CODE.          
      *****ENDIF                                                        
           MOVE        CTF-TR-ACC-NO-1     TO    LPB-ACC-NO.            
"          MOVE        CTF-LN-ID-NO        TO    LPB-LN-ID-NO.          
           MOVE        CTF-W-LOG-AMT       TO    LPB-DECR-AMT.          
           MOVE       '0000'               TO    LPB-RC-CODE.           
050614*---------------放款帳號／科目／本金／利息／違約金              
"          MOVE        CTF-ACC-NO           TO   LPB-LN-ACC-NO.         
"          MOVE        LNL-ACC-CODE         TO   LPB-ACC-CODE.          
"          MOVE        CTF-RECV-AMT         TO   LPB-RTRN-AMT.          
"          MOVE        CTF-INT-AMT          TO   LPB-INT-AMT.           
"          MOVE        CTF-FINE             TO   LPB-FINE-AMT.          
050122     PERFORM     9401-PB-LINK-RTN          THRU  9401-EXIT.       
           MOVE        WK-TXN-ID-CODE       TO   TWA-TXN-ID-CODE.       
931216 750-EXIT.                                                        
           EXIT.                                                        
------*-----------------------------------------------------------------
       800-TXN-END-RTN.
960521     IF          ( LNL-ACC-CODE          = 47 OR 57 )             
971222            AND  ( CTF-CAPTL-PURPOSE     = 50 )                   
"          THEN                                                         
"                 PERFORM  800A-OUTPUT-TXN-INFO-RTN  THRU  800A-EXIT.   
           PERFORM     9004-LN-CLOSE-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
"     *------------/單筆現金交易≧伍拾萬－認證大額通貨交易登記表/----*
"          IF      ( TWA-TXN-ID-CODE-1 = 671 OR 672)                    
"              AND ( CTF-W-LOG-AMT NOT < 500000 )                       
"              AND ( NOT TWA-C-TXN-REVERSE      )                       
"          THEN                                                         
"                    MOVE   '9201'           TO  LINK-CF-TXN-ID         
"                    MOVE    CTF-ACC-NO      TO  LINK-P-ACC-NO          
"                    MOVE    LNM-CREAT-LNM-DATE  TO  LINK-P-OPEN-DAY    
"                    MOVE    WK-ID-NO        TO  LINK-P-ID-NO           
041211               MOVE    WK-LNM-CFM-SER  TO  LINK-P-SER             
080425               MOVE    TWA-TXN-ID-CODE    TO  LINK-P-ID           
080425               MOVE    CTF-W-LOG-AMT      TO  WK-9201-AMT         
080425               MOVE    WK-9201-AMT        TO  LINK-P-AMT          
981119               MOVE    0               TO  LINK-P-DB-CR-CODE      
"                    MOVE    2               TO  TWA-INTER-AP-PROC      
"                    CALL   'UCPAPRQT'    USING  TWA,  LINK-CF-AREA.    
"     *    ENDIF                                                        
"          IF      ( TWA-TXN-ID-CODE-1 = 674 OR 675 )                   
"              AND ( CTF-RECV-CASH-AMT NOT < 500000 )                   
"              AND ( NOT TWA-C-TXN-REVERSE      )                       
"          THEN                                                         
"                    MOVE   '9201'           TO  LINK-CF-TXN-ID         
"                    MOVE    CTF-ACC-NO      TO  LINK-P-ACC-NO          
"                    MOVE    LNM-CREAT-LNM-DATE  TO  LINK-P-OPEN-DAY    
"                    MOVE    WK-ID-NO        TO  LINK-P-ID-NO           
041211               MOVE    WK-LNM-CFM-SER  TO  LINK-P-SER             
080425               MOVE    TWA-TXN-ID-CODE    TO  LINK-P-ID           
080425               MOVE    CTF-RECV-CASH-AMT  TO  WK-9201-AMT         
080425               MOVE    WK-9201-AMT        TO  LINK-P-AMT          
981119               MOVE    0               TO  LINK-P-DB-CR-CODE      
"                    MOVE    2               TO  TWA-INTER-AP-PROC      
"                    CALL   'UCPAPRQT'    USING  TWA,  LINK-CF-AREA.    
980731*    ENDIF                                                        
           PERFORM     9160-LN-ACC-TXN-END-RTN.
       800-EXIT.
           EXIT.
------*-------------連動轉銷呆帳列印----------------------------------
960521 800A-OUTPUT-TXN-INFO-RTN.                                        
"          MOVE      'W610'              TO        MSG-P-OUT-CODE.      
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"          MOVE        SPACE               TO      MSG-P-OUT.           
"          MOVE        'T635'              TO      MSG-P-OUT-CODE.      
080116     MOVE        200                 TO      MSG-P-LENGTH.        
           MOVE        MSG-M-DOCUMENT      TO      MSG-P-OUT-TM-TYPE.   
      *---------------戶名／帳務別／機號／交易序號／交易代號          
971222     MOVE        WK-CUST-NAME         TO   T635-CUST-NAME.        
"          IF        ( TWA-C-TXN-NOT-REVERSE)                           
"          THEN                                                         
"                      MOVE    CTF-M-TR          TO   T635-AMT-TYPE     
"          ELSE                                                         
"                      MOVE    CTF-M-TR-REVERSE  TO   T635-AMT-TYPE.    
      *****ENDIF                                                        
"          MOVE        TWA-TM-CODE          TO   T635-TM-CODE.          
"          MOVE        TWA-ACC-TXN-SEQ-NO   TO   T635-TXN-SEQ-NO.       
"          MOVE        '6481'               TO   T635-TXN-ID-CODE.      
"     *---------------其他資訊                                        
961206     MOVE   CTF-ACC-NO               TO  MSG-P-SP-ACC-NO.         
"          MOVE   LNTR-P-SP-OLD-LN-ID-NO   TO MSG-P-SP-OLD-LN-ID-NO.    
"          MOVE   LNTR-P-SP-ACC-CODE       TO MSG-P-SP-ACC-CODE.        
"          MOVE   LNTR-P-SP-CURRENCY-CODE  TO MSG-P-SP-CURRENCY-CODE.   
"          MOVE   LNTR-P-SP-NEW-LN-ID-NO   TO MSG-P-SP-NEW-LN-ID-NO     
"          MOVE   LNTR-P-SP-AMT            TO MSG-P-SP-AMT.             
"          MOVE   LNTR-P-SP-DUE-DATE       TO MSG-P-SP-DUE-DATE.        
      *---------------主管卡／交易日期時間／櫃員卡                    
"          MOVE        LNTR-P-SP-SUP-CODE   TO   MSG-P-SP-SUP-CODE.     
"          MOVE        '主管:'            TO   T635-SUP-TITLE         
080116     IF        ( LNTR-P-SP-SUP-CODE  NUMERIC )                    
"                AND ( LNTR-P-SP-SUP-CODE  >  ZEROS )                   
"          THEN                                                         
"                      MOVE  LNTR-P-SP-SUP-CODE  TO   T635-SUP-CODE     
"                      MOVE  'SUPNAME   '        TO   T635-SUP-NAME     
"          ELSE                                                         
"                      MOVE  '**'                TO   T635-SUP-CODE     
"                      MOVE  SPACES              TO   T635-SUP-NAME.    
      *****ENDIF                                                        
      *---------------交易日／時間／櫃員卡                            
961206     MOVE   TWA-TXN-DAT7             TO MSG-P-SP-TXN-DATE.        
"          MOVE        TWA-TXN-TIME-HH     TO MSG-P-SP-TXN-TIME-HH.     
"          MOVE        ':'                 TO MSG-P-SP-FILLER1.         
"          MOVE        TWA-TXN-TIME-MM     TO MSG-P-SP-TXN-TIME-MM.     
"          MOVE        ':'                 TO MSG-P-SP-FILLER2.         
"          MOVE        TWA-TXN-TIME-SS     TO MSG-P-SP-TXN-TIME-SS.     
080116     MOVE        '櫃員:'            TO   T635-TELLER-TITLE.     
"          MOVE        TWA-TELLER-CODE      TO   T635-TELLER-CODE.      
"          MOVE        'TELNAME   '         TO   T635-TELLER-NAME.      
      *---------------貸款種類／逾期註記／免責債權                    
960827     MOVE        LNTR-KIND           TO      MSG-P-SP-BAD-TYPE.   
"          MOVE        LNTR-MARK           TO      MSG-P-SP-BAD-MARK.   
050302     MOVE        LNTR-RELIEF-TYPE     TO   T635-RELIEF-TYPE.      
      *---------------輸出處理                                        
"          CALL      'UCPWRITE'          USING   TWA  MSG-P-OUT.        
"          PERFORM   9147-LN-UCP-WRITE-ERR-HDL-RTN.                     
"      800A-EXIT.                                                       
"           EXIT.                                                       
------*---------------〔０８〕計算月底日------------------------------
040317 808-DAT-08-CAL-RTN.                                              
"          PERFORM     9171-LN-INT-AREA-CLEAR.                          
"          MOVE        'E'                  TO   INT-F-SERV-TYPE-DATE.  
"          MOVE        '08'                 TO   DAT-P-FUN-CODE.        
"          MOVE        TWA-TXN-ID-CODE      TO   INT-F-TXN-ID-CODE.     
"          MOVE        TWA-TXN-DAT7         TO   INT-F-TXN-DATE-R.      
"          MOVE        TWA-BR-CODE          TO   INT-F-BR-CODE.         
040317     MOVE        WK-DAT-BEG-DATE      TO   DAT-P-BEG-DATE.        
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
"      808-EXIT.                                                        
"          EXIT.                                                        
------*---------------〔１５〕計算起迄期間期數------------------------
040317 815-DAT-15-CAL-RTN.                                              
"          PERFORM     9171-LN-INT-AREA-CLEAR.                          
"          MOVE        'E'                  TO   INT-F-SERV-TYPE-DATE.  
"          MOVE        '15'                 TO   DAT-P-FUN-CODE.        
"          MOVE        TWA-TXN-ID-CODE      TO   INT-F-TXN-ID-CODE.     
"          MOVE        TWA-TXN-DAT7         TO   INT-F-TXN-DATE-R.      
"          MOVE        TWA-BR-CODE          TO   INT-F-BR-CODE.         
040317     MOVE        WK-DAT-BEG-DATE      TO   DAT-P-BEG-DATE.        
"          MOVE        WK-DAT-END-DATE      TO   DAT-P-END-DATE         
"          MOVE        WK-DAT-DAYS          TO   DAT-P-DAYS.            
"          MOVE        WK-DAT-TERM-UNIT     TO   DAT-P-TERM-UNIT.       
110926     PERFORM     978-NACINT7-DATE-RTN      THRU  978-EXIT.        
"      815-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
       9000-PROG-INIT-RTN.   COPY LNK000.                               
       9002-LN-OPEN-RTN.     COPY LNK002.                               
       9004-LN-CLOSE-RTN.    COPY LNK004.                               
       9016-LN-DBS-M2-RTN.   COPY LNK016.                               
       9016-LN-DBS-M2C-RTN.  COPY LNK016                                
                   REPLACING CTF-ACC-NO     BY   W-ACC-NO.              
       9020-LN-DBS-MF-RTN.   COPY LNK020.                               
       9020-LN-DBS-MF1-RTN.  COPY LNK020                                
                   REPLACING CTF-ACC-NO     BY   W-ACC-NO.              
       9026-LN-DBS-P2-RTN.   COPY LNK026.                               
       9026-LN-DBS-P2TT-RTN. COPY LNK026                                
                   REPLACING CTF-APRV-NO    BY   WS-CTF-APRV-NO.        
       9026-LN-DBS-P2T-RTN.  COPY LNK026                                
                   REPLACING CTF-APRV-NO    BY   W-LNP-APRV-NO.         
       9032-LN-DBS-PF-RTN.   COPY LNK032.                               
       9032-LN-DBS-PFT-RTN.  COPY LNK032                                
                   REPLACING CTF-APRV-NO    BY   WS-CTF-APRV-NO.        
       9032-LN-DBS-PF1-RTN.  COPY LNK032                                
                   REPLACING CTF-APRV-NO    BY   W-LNP-APRV-NO.         
       9045-LN-DBS-L1K-RTN.  COPY LNK045    REPLACING                   
                   TWA-M-READ-NEXT-UPD-PAR  BY  TWA-M-READ-UPDATE-F-SEG.
       9047-LN-DBS-L2-RTN.   COPY LNK047.                               
       9050-LN-DBS-L51-RTN.  COPY LNK050                                
                   REPLACING SPACE          BY   LNL-LN-SER-NO.         
       9058-LN-DBS-LD-RTN.   COPY LNK058.                               
       9060-LN-DBS-LF-RTN.   COPY LNK060.                               
       9076-LN-DBS-R2-RTN.   COPY LNK076.                               
       9079-LN-DBS-R5-RTN.   COPY LNK079.                               
       9081-LN-DBS-R7-RTN.   COPY LNK081.                               
       9091-LN-DBS-I2-RTN.   COPY LNK091.                               
       9094-LN-DBS-I5-RTN.   COPY LNK094.                               
       9096-LN-DBS-I7-RTN.   COPY LNK096.                               
------*---------------檔案存取錯誤處理--------------------------------
960521 910-NOT-OPEN.                                                    
"          MOVE      'D176'              TO      MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      910-OTHER-ERROR.                                                 
"          MOVE      'D898'              TO      MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      910-NOT-FND.                                                     
"          MOVE      'D898'              TO      MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
000311 910-DUPREC.                                                      
"          MOVE      'D612'              TO      MSG-P-OUT-CODE.        
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
------*--------------檔案存取錯誤處理（中文化）-----------------------
930730 910-FILE-NOTOPEN.                                                
"          MOVE        'M612A'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案未開啟'       TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-NOTFND.                                                 
"          MOVE        'M612B'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案資料不存在'   TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-DUPREC.                                                 
"          MOVE        'M612C'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案紀錄重複'     TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-NOSPACE.                                                
"          MOVE        'M612D'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案空間不夠'     TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-DSIDERR.                                                
"          MOVE        'M612E'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案未定義ＦＣＴ' TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-LENGERR.                                                
"          MOVE        'M612F'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案長度錯誤'     TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-ILLOGIC.                                                
"          MOVE        'M612G'              TO   MSG-FILE-ERR-CODE.     
"          MOVE  '檔案存取ＩＬＬＯＧＩＣ' TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-INVREQ.                                                 
"          MOVE        'M612H'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案存取順序錯誤' TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-DUPKEY.                                                 
"          MOVE        'M612I'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案路徑檔重複'   TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
"      910-FILE-OTHER-ERROR.                                            
"          MOVE        'M612J'              TO   MSG-FILE-ERR-CODE.     
"          MOVE        '檔案存取錯誤'     TO   MSG-FILE-ERROR-RSN.    
040317     GO   TO     910-OUTPUT-END-RTN.                              
040317*---------------檔案存取錯誤輸出                                
"      910-OUTPUT-END-RTN.                                              
"          MOVE        SPACES               TO   MSG-T282.              
"          MOVE        MSG-FILE-LINE1       TO   MSG-T282-LINE1.        
"          MOVE        MSG-FILE-LINE2       TO   MSG-T282-LINE2.        
"          PERFORM     9146A-CHINESE-MSG-RTN     THRU  9146A-EXIT.      
"      910-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
980506 9102-SEAH-FINE-DAYS-RTN.        COPY LNK102N.                    
810821 9104-TEL-WRITE-RTN.             COPY LNKT01.                     
       9105-LN-DBS-ERR-HDL-RTN.        COPY LNK105.                     
       9106-LN-DBS-ERR-HDL-RTN-BOTH.   COPY LNK106.                     
------*--------------檔案存取錯誤處理（會RECOVERY）-----------------
930730 911-FILE-NOTOPEN.                                                
           MOVE        'D610'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-NOTFND.                                                 
           MOVE        'D611'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-DUPREC.                                                 
           MOVE        'D612'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-NOSPACE.                                                
           MOVE        'D613'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-DSIDERR.                                                
           MOVE        'D614'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-LENGERR.                                                
           MOVE        'D615'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-ILLOGIC.                                                
           MOVE        'D616'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-INVREQ.                                                 
           MOVE        'D617'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
930730 911-FILE-OTHER-ERROR.                                            
           MOVE        'D619'     TO   MSG-P-OUT-CODE.                  
           PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
------*-----------------------------------------------------------------
       9112-LNM-STATUS-CHK-RTN-A.      COPY LNK112.                     
       9114-SET-APRV-NO-RTN.           COPY LNK114.                     
------*---------------檢驗LNP-STATUS ---------------------------------
       9116-LNP-STATUS-CHK-RTN-N.                                       
           PERFORM     9116-CASE-BEGIN     THRU    9116-CASE-EXIT.
       9116-DUMMY.
           EXIT.
       9116-CASE-BEGIN.
           IF          (LNP-C-NORMAL)      GO TO   9116-CASE-NORMAL.
           IF          (LNP-C-AWAIT-GURN)  GO TO   9116-CASE-GURN.
           IF          (LNP-C-ERR-DLT)     GO TO   9116-CASE-DEL.
           GO TO       9116-CASE-ERR.
       9116-CASE-NORMAL.
           MOVE        '0000'              TO      MSG-P-OUT-CODE.
           GO   TO     9116-CASE-END.
       9116-CASE-GURN.
920227*---------------開放「未鍵保證人」也可收息                      
920227     MOVE        '0000'              TO      MSG-P-OUT-CODE.      
           GO   TO     9116-CASE-END.
       9116-CASE-DEL.
           MOVE        'M653'              TO      MSG-P-OUT-CODE.
           GO   TO     9116-CASE-END.
       9116-CASE-ERR.
           MOVE        'S602'              TO      MSG-P-OUT-CODE.
           GO   TO     9116-CASE-END.
       9116-CASE-END.
           PERFORM     9146-LN-ERR-MSG-OUT-RTN   THRU  9146-DUMMY.
           GO   TO     9116-CASE-EXIT.
       9116-CASE-EXIT.
            EXIT.                                                       
       9118-LNP-STATUS-CHK-RTN-N-G. COPY LNK118.                        
       9121-LNP-STOP-CHK-RTN-N.     COPY LNK121.                        
       9125-UPDATE-CTZN-RTN.        COPY LNK125.                        
------*-----------------------------------------------------------------
       9129-LNL-STATUS-CHK-RTN-N-W.
           IF        ( LNL-C-NORMAL OR LNL-C-NEW )                      
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-RTRN )                                     
                       MOVE    'M60W'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-TR )                                       
                       MOVE    'M60X'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-TR-OVER )                                  
                       MOVE    'M60Y'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-MVOUT )                                    
                       MOVE    'M60Z'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
                 AND ( LNP-ACC-CODE  =  '21' OR '22' OR '24' )          
                 AND ( LNP-LN-CODE   =  '70' OR '71' )                  
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
                       MOVE    'M61A'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-ERR-DLT )                                  
                       MOVE    'M61B'            TO   MSG-P-OUT-CODE    
           ELSE                                                         
                       MOVE    'S606'            TO   MSG-P-OUT-CODE.   
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN THRU 9146-DUMMY. 
      *****ENDIF                                                        
           PERFORM     9146-LN-ERR-MSG-OUT-RTN      THRU  9146-DUMMY.   
       9129-DUMMY.
            EXIT.                                                       
------*-----------------------------------------------------------------
       9131-LNL-STATUS-CHK-RTN-WE-EE.
           IF        ( LNL-STATUS = '0' OR '2' OR '3' OR '4' OR '5' )   
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-NEW )                                      
                       MOVE    'M60V'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
810421           AND ( LNP-ACC-CODE  =  '21' OR '22' OR '24' )          
810421           AND ( LNP-LN-CODE   =  '70' OR '71' )                  
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
                       MOVE    'M61A'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-ERR-DLT )                                  
                       MOVE    'M61B'            TO   MSG-P-OUT-CODE    
           ELSE                                                         
                       MOVE    'S606'            TO   MSG-P-OUT-CODE.   
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN THRU 9146-DUMMY. 
      *****ENDIF                                                        
           PERFORM     9146-LN-ERR-MSG-OUT-RTN      THRU  9146-DUMMY.   
       9131-DUMMY.
            EXIT.                                                       
------*-----------------------------------------------------------------
       9134-LNL-STATUS-CHK-RTN-ERR-E.
           IF        ( LNL-STATUS  =  '0' OR '1' OR '2' OR '3'          
                                   OR '4' OR '5' )                      
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-ERR-DLT )                                  
                       MOVE    'M61B'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
810421           AND ( LNP-ACC-CODE  =  '21' OR '22' OR '24' )          
810421           AND ( LNP-LN-CODE   =  '70' OR '71' )                  
                       MOVE    '0000'            TO   MSG-P-OUT-CODE    
           ELSE IF   ( LNL-C-CONVERT )                                  
                       MOVE    'M61A'            TO   MSG-P-OUT-CODE    
           ELSE                                                         
                       MOVE    'S606'            TO   MSG-P-OUT-CODE.   
                       PERFORM 9146-LN-ERR-MSG-OUT-RTN THRU 9146-DUMMY. 
      *****ENDIF                                                        
           PERFORM     9146-LN-ERR-MSG-OUT-RTN      THRU  9146-DUMMY.   
       9134-DUMMY.                                                      
            EXIT.                                                       
------*---------------建檔：ＬＮＲ------------------------------------
940506 9143-LN-ESTB-LNR-RTN.                                            
"          MOVE        ZEROS                TO   LNR.                   
"          MOVE        ZEROS                TO   LNR-RTRNABL-AMT.       
961206     MOVE        TWA-TXN-DAT7         TO   LNR-RTRN-DATE.         
"          MOVE        CTF-RECV-AMT         TO   LNR-RTRN-AMT.          
"          COMPUTE     LNR-RTRN-BAL    =    LNL-LN-BAL - CTF-RECV-AMT.  
021210*---------------設定：〔ＬＮＭ帳戶餘額異動數〕                  
"          IF        ( LNL-LN-BAL  <  ZEROS )                           
"                      MOVE    ZEROS             TO   WK-LNM-TXN-AMT    
"          ELSE IF   ( LNR-RTRN-BAL  <  ZEROS )                         
"                      MOVE    LNL-LN-BAL        TO   WK-LNM-TXN-AMT    
"          ELSE                                                         
"                      MOVE    CTF-RECV-AMT      TO   WK-LNM-TXN-AMT.   
      *****ENDIF                                                        
961206     MOVE        LNR-M-NORMAL         TO   LNR-STATUS.            
"          IF        ( SW-C-FINE-YES )                                  
"          THEN                                                         
"                      MOVE    1       TO   LNR-OVERDUE-RTRN-STATUS.    
      *****ENDIF                                                        
"          IF        ( TWA-C-SUP-ON )                                   
"          THEN                                                         
"                      MOVE    CTF-SUP-CODE      TO  LNR-SUP-CODE.      
      *****ENDIF                                                        
"     *---------------應攤還本金                                      
"          MOVE        LNR-RTRN-AMT              TO   LNR-RTRNABL-AMT.  
040317     IF        ( W-CTF-RECV-TYPE    =  '2' OR '7' )               
"                AND ( LNR-RTRNABL-AMT  <  WK7-RTRNABL-AMT )            
"          THEN                                                         
"                      MOVE    WK7-RTRNABL-AMT   TO   LNR-RTRNABL-AMT.  
      *****ENDIF                                                        
971222*---------------收回原因／免責債權種類                          
"          MOVE        CTF-CAPTL-PURPOSE    TO   LNR-RTRN-RSN.          
980720     MOVE        CTF-RELIEF-TYPE      TO   LNR-RELIEF-TYPE.       
040317*---------------中途攤還相關欄位                                
"          MOVE        ZEROS                TO   LNR-MID-MARK.          
"          MOVE        ZEROS                TO   LNR-MID-AMT.           
"          PERFORM     9143A-LNR-MID-HDL-RTN     THRU  9143A-EXIT.      
110509*---------------寫檔（按季攤還且攤還本金為０＝＞不寫）          
"          IF        ( CTF-RECV-AMT  >  ZEROS )                         
"          THEN                                                         
"             PERFORM  9081-LN-DBS-R7-RTN                               
"             PERFORM  9105-LN-DBS-ERR-HDL-RTN                          
"             ADD      1                    TO   LNL-LNR-NO.            
      *****ENDIF                                                        
"      9143-EXIT.                                                       
"           EXIT.                                                       
------*---------------ＬＮＲ〔中途攤還〕相關欄位設定------------------
040317 9143A-LNR-MID-HDL-RTN.                                           
040317*-------------【１】提前清償                                    
"          IF       ( CTF-RECV-TYPE  =  '1' )                           
"               AND ( TWA-TXN-DAT7   <  WKS-DUE-DATE )                  
"                     MOVE    '1'               TO   LNR-MID-MARK       
"                     MOVE    CTF-RECV-AMT      TO   LNR-MID-AMT        
040317*-------------【２】只繳息或寬限期內案件／中途攤還              
"          ELSE IF  ( WKS-REAL-RTRN-CODE  =  '0' )                      
"               AND ( CTF-RECV-TYPE       =  '2' )                      
"                     MOVE    '2'               TO   LNR-MID-MARK       
"                     MOVE    CTF-RECV-AMT      TO   LNR-MID-AMT        
"          ELSE IF  ( WKS-REAL-RTRN-CODE  =  '0' )                      
"                     NEXT    SENTENCE                                  
040327*-------------【３】攤還案件／〔４／７〕中途攤還                
"          ELSE IF  ( CTF-RECV-TYPE  =  '4' OR '7' )                    
"                     MOVE    '3'               TO   LNR-MID-MARK       
"                     MOVE    CTF-RECV-AMT      TO   LNR-MID-AMT        
141216*-------------【４】攤還案件〔２〕攤還且＞應攤還金額（攤還方式１
"          ELSE IF  ( CTF-RECV-TYPE  =  '2' )                           
"               AND ( LNP-RTRN-CODE  =  '1' )                           
"               AND ( CTF-RECV-AMT   >  LNL-EACH-RTRN-AMT )             
"                     MOVE    '4'               TO   LNR-MID-MARK       
"                     COMPUTE LNR-MID-AMT   =   CTF-RECV-AMT            
"                                            -  LNL-EACH-RTRN-AMT       
"          ELSE IF  ( CTF-RECV-TYPE  =  '2' )                           
"               AND ( LNP-RTRN-CODE  =  '1' )                           
"               AND (( CTF-RECV-AMT  =  LNL-EACH-RTRN-AMT )             
"                OR  ( CTF-RECV-AMT  <  LNL-EACH-RTRN-AMT ))            
"                     NEXT    SENTENCE                                  
"          ELSE IF  ( CTF-RECV-TYPE  =  '2' )                           
"               AND ( LNP-RTRN-CODE  =  '9' )                           
"               AND ( LNP-AFTER-TERM-RTRN-TYPE  = '1'  )                
"               AND ( LNP-RTRN-INTVL            = '01' )                
"               AND (( CTF-RECV-AMT  =  LNL-EACH-RTRN-AMT )             
"                OR  ( CTF-RECV-AMT  <  LNL-EACH-RTRN-AMT ))            
"                     NEXT    SENTENCE                                  
"          ELSE IF  ( CTF-RECV-TYPE  =  '2' )                           
"               AND ( LNP-RTRN-CODE  =  '9' )                           
"               AND ( LNP-AFTER-TERM-RTRN-TYPE  = '1'  )                
"               AND ( LNP-RTRN-INTVL            = '01' )                
"               AND ( CTF-RECV-AMT   >  LNL-EACH-RTRN-AMT )             
"                     MOVE    '4'               TO   LNR-MID-MARK       
"                     COMPUTE LNR-MID-AMT   =   CTF-RECV-AMT            
"                                            -  LNL-EACH-RTRN-AMT       
040327*-------------【４】攤還案件〔２〕攤還且＞應攤還金額（攤還方式２
"          ELSE IF  ( CTF-RECV-TYPE  =  '2' )                           
"               AND ( CTF-RECV-AMT   > INT-P-PRNCPAL-THIS-INTVL )       
"                     MOVE    '4'               TO   LNR-MID-MARK       
"                     COMPUTE LNR-MID-AMT   =   CTF-RECV-AMT            
"                                            -  INT-P-PRNCPAL-THIS-INTVL
"          ELSE                                                         
"                     NEXT    SENTENCE.                                 
      *****ENDIF                                                        
"      9143A-EXIT.                                                      
"            EXIT.                                                      
------*---------------產生收息紀錄主檔ＬＮＩ執行段落------------------
       9145-LN-ESTB-FINE-LNI-RTN.                                       
           MOVE        ZERO                TO      LNI.
961206     MOVE        TWA-TXN-DAT7        TO      LNI-RECV-INT-DATE-R. 
791114     MOVE        LNL-RECV-INT-DATE-R TO      LNI-INT-BGN-DATE-R.
961206     MOVE        TWA-TXN-DAT7        TO      LNI-INT-END-DATE-R.  
           IF          WS-INT              =       0                 OR
970609                 (CTF-INT-BGN-DATE-N-R  >   WKS-DUE-DATE    OR    
"                       CTF-INT-BGN-DATE-N-R  =    WKS-DUE-DATE)        
           THEN
970609       MOVE      CTF-INT-BGN-DATE-N-R  TO    LNI-INT-BGN-DATE-R.  
      *****ENDIF                                                        
           MOVE        INT-P-INT-DELAY-INTVL-DD  TO  LNI-DAYS.
           MOVE        INT-P-INT-DELAY-RATE        TO  LNI-INT-RATE.
           IF        ( WKS-C-FIX-ANNUITY  )                             
                 AND ( NOT  CTF-C-TR-OVER )                             
110926           AND ( INT-P-PRNCPAL-THIS-INTVL  >  ZEROS )             
"                  MOVE INT-P-PRNCPAL-THIS-INTVL TO   LNI-FOR-INT-AMT   
"          ELSE IF   ( WKS-C-FIX-ANNUITY  )                             
"                AND ( NOT  CTF-C-TR-OVER )                             
"                  MOVE INT-P-INT-THIS-INTVL     TO   LNI-FOR-INT-AMT   
           ELSE
                   MOVE LNL-LN-BAL               TO   LNI-FOR-INT-AMT.  
      *****ENDIF                                                        
           MOVE        LNI-M-FINE-OVER     TO      LNI-INT-CHAR.
           MOVE        LNI-M-NORMAL        TO      LNI-STATUS.
           MOVE        WS-INT-FINE         TO      LNI-RECVABL-AMT.
           MOVE        CTF-FINE            TO      LNI-RECVED-AMT.
           IF        ( WS-INT = 0 ) AND ( CTF-INT-AMT = 0 )             
           THEN                                                         
                       MOVE    CTF-FINE     TO   LNI-INT-FINE           
           ELSE                                                         
                       MOVE    0            TO   LNI-INT-FINE.          
      *****ENDIF                                                        
           MOVE        CTF-SUP-CODE        TO      LNI-SUP-CODE.
901109     MOVE        ZEROS TO   LNI-REMARK     LNI-LABOR-DELAY-INT.   
991203     MOVE        +0    TO   LNI-RTRN-AMT   LNI-NOT-YET-RTRN-FREE  
"                                 LNI-RTRN-FREE.                        
           PERFORM     9096-LN-DBS-I7-RTN.
           PERFORM     9105-LN-DBS-ERR-HDL-RTN.
           ADD         1                   TO      LNL-LNI-NO.
------*-----------------------------------------------------------------
       9146-LN-ERR-MSG-OUT-RTN.        COPY LNK146.                     
------*----------------中文錯誤訊息輸出-------------------------------
030224 9146A-CHINESE-MSG-RTN.          COPY LNK146A.                    
       9147-LN-UCP-WRITE-ERR-HDL-RTN.  COPY LNK147.                     
       9148-LN-TXN-LOG-RTN.            COPY LNK148.                     
       9149-INT-ACC-TR-RTN.            COPY LNK149.                     
       9160-LN-ACC-TXN-END-RTN.        COPY LNK160.                     
       9162-LN-BIF-ERR-HDL-RTN.        COPY LNK162.                     
       9163-LN-RATE-SEA-ERR-HDL-RTN.   COPY LNK163.                     
       9164-LN-DATE-ERR-HDL-RTN.       COPY LNK164.                     
       9165-INT-SUB-ERR-HDL-RTN.       COPY LNK165.                     
       9167-LN-INT-DATE-ADJ-RTN.
------****  RECV  INT  DATE  ADJ ROUTINE  ****--------------------------
           PERFORM     9171-LN-INT-AREA-CLEAR.
A91114     MOVE        INT-M-EXEC-LN-D     TO      INT-F-SERV-TYPE-DATE.
           MOVE        TWA-TXN-ID-CODE     TO      INT-F-TXN-ID-CODE.
961206     MOVE        TWA-TXN-DAT7        TO      INT-F-TXN-DATE-R.    
           MOVE        TWA-BR-CODE         TO      INT-F-BR-CODE.
           MOVE        LNL-RECV-INT-DATE   TO      DAT-P-BEG-DATE.
           MOVE        LNL-RECV-INT-DAY    TO      DAT-P-L-TXN-DD.
           MOVE        DAT-M-FUN-3         TO      DAT-P-FUN-CODE.
           IF    ( LNP-C-INT-RECV-MON1 )   MOVE 01 TO DAT-P-TERM-MM.    
           IF    ( LNP-C-INT-RECV-MON3 )   MOVE 03 TO DAT-P-TERM-MM.    
           IF    ( LNP-C-INT-RECV-MON6 )   MOVE 06 TO DAT-P-TERM-MM.    
           IF    ( LNP-C-INT-RECV-YEAR1 )  MOVE 12 TO DAT-P-TERM-MM.    
980506     EXEC CICS LINK PROGRAM ('NACINT7')                           
"                    COMMAREA (INT-GROUP)                               
"                    LENGTH (INT-LENGTH)                                
"          END-EXEC.                                                    
           PERFORM     9164-LN-DATE-ERR-HDL-RTN.
791114     MOVE    LNL-RECV-INT-DATE-R   TO    WKS-RECV-INT-DATE.
           MOVE        DAT-P-END-DATE    TO    LNL-RECV-INT-DATE.
       9167-EXIT-RTN.
            EXIT.
------*-----------------------------------------------------------------
980506 9168-LN-RTRN-DATE-ADJ-RTN.      COPY LNK168N                     
970609             REPLACING TWA-TXN-DD7 BY LNL-NEXT-RTRN-DD.           
980506 9169-ADJ-INT-DATE-RTN.          COPY LNK169N                     
970609             REPLACING TWA-TXN-DD7 BY LNL-1ST-LN-DD.              
       917-ERR-MSG-OUT-RTN.            COPY PBK17.                      
       9171-LN-INT-AREA-CLEAR.         COPY LNK171.                     
       9175-LN-BIF-CLEAR-RTN.          COPY LNK175.                     
       9175-END-EXIT.   EXIT.                                           
       9181-LN-LNI-CLEAR-RTN.          COPY LNK181.                     
------*---------------讀取活力貸控制檔--------------------------------
890309 9181-READ-PBHOUS-RTN.                                            
"          MOVE   '（PBHOUSE活力貸控制檔）'  TO   MSG-FILE-LINE2.   
"          EXEC CICS   HANDLE CONDITION                                 
"                             NOTOPEN(910-FILE-NOTOPEN)                 
"                             NOTFND(910-FILE-NOTFND)                   
"                             ENDFILE(910-FILE-NOTFND)                  
"                             ERROR(910-FILE-OTHER-ERROR)               
"                      END-EXEC.                                        
"          MOVE        LNM-CUST-ID        TO      PB-H-ID-NO.           
090701     MOVE        LNM-AGENT-BR       TO      PB-H-BR-CODE.         
"          EXEC  CICS  READ  DATASET('PBHOUSE')                         
"                            INTO(PB-H-REC)                             
"                            LENGTH(PB-H-LEN)                           
"                            RIDFLD(PB-H-KEY)                           
"                      END-EXEC.                                        
"      9181-EXIT.                                                       
"           EXIT.                                                       
------*-----------------------------------------------------------------
"      9181A-READ-PBHOUS-RTN.                                           
"          EXEC CICS   HANDLE CONDITION                                 
"                             NOTOPEN(910-FILE-NOTOPEN)                 
"                             NOTFND(910-FILE-NOTFND)                   
"                             ENDFILE(910-FILE-NOTFND)                  
"                             ERROR(910-FILE-OTHER-ERROR)               
"                      END-EXEC.                                        
"          MOVE        LNM-CUST-ID        TO      PB-H-ID-NO.           
090701     MOVE        LNM-AGENT-BR       TO      PB-H-BR-CODE.         
"          EXEC  CICS  READ  DATASET('PBHOUSE')                         
"                            INTO(PB-H-REC)                             
"                            LENGTH(PB-H-LEN)                           
"                            RIDFLD(PB-H-KEY)                           
"                      END-EXEC.                                        
"      9181A-EXIT.                                                      
"           EXIT.                                                       
------*--------------檢核：活力貸控管檔PBHOUSE額度金額--------------
"      91811-CHECK-TOT-RNT.                                             
"          COMPUTE  WK-PB-HOUSE-AMT = PB-H-LNL-AMT(II)                  
"                                   + PB-H-PB-TREATY-AMT(II).           
"          IF       WK-PB-HOUSE-AMT  NOT  =  PB-H-APRV-AMT(II)          
"          THEN                                                         
"          MOVE    'M60J'    TO    MSG-P-OUT-CODE.                      
"          PERFORM   9146-LN-ERR-MSG-OUT-RTN.                           
"      91811-EXIT.                                                      
"           EXIT.                                                       
------*---------------連結更新ＰＢＭ活力貸短擔可動用額度--------------
"      9182-LINK-PB-RTN.                                                
"            EXEC  CICS  LINK  PROGRAM('PBLHOUSE')                      
"                              COMMAREA(COMM-PB-AREA)                   
"                              LENGTH(60)                               
"                              END-EXEC.                                
"          IF        ( COMM-RETURN-CODE  =   ZEROS OR SPACES )          
"                      GO  TO  9182-EXIT                                
"          ELSE IF   ( COMM-RETURN-CODE  =  'M008' OR 'M009' OR 'M194'  
"                                        OR 'M624' OR 'M629' OR 'M636' )
"                      MOVE    COMM-RETURN-CODE  TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN                  
"          ELSE                                                         
"                      MOVE    'M809'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
      *****ENDIF                                                        
"      9182-EXIT.                                                       
"           EXIT.                                                       
------*--------------處理：活力貸控管檔ＰＢＨＯＵＳＥ（正常）---------
"      9184-UPD-TOT-FIL-RTN.                                            
110926*---------------讀ＰＢＨＯＵＳＥ檔                              
"          PERFORM     9184A-PBHOUSE-READ-RTN    THRU  9184A-EXIT.      
090701*---------------計算：現欠餘額、可用額度／合理性檢核            
"          COMPUTE     PB-H-LNL-AMT(II) =   PB-H-LNL-AMT(II)            
"                                         - CTF-RECV-AMT.               
"          COMPUTE     PB-H-PB-TREATY-AMT(II) =  PB-H-PB-TREATY-AMT(II) 
"                                              + CTF-RECV-AMT.          
"          PERFORM     91811-CHECK-TOT-RNT       THRU  91811-EXIT.      
090701*---------------更新檔案                                        
"          EXEC        CICS  REWRITE   DATASET('PBHOUSE')               
"                                      FROM(PB-H-REC)                   
"                                      LENGTH(PB-H-LEN)                 
"                      END-EXEC.                                        
"      9184-EXIT.                                                       
"           EXIT.                                                       
------*--------------活力貸控管檔ＰＢＨＯＵＳＥ：讀檔處理-------------
110926 9184A-PBHOUSE-READ-RTN.                                          
"          EXEC        CICS  HANDLE    CONDITION                        
"                                      NOTOPEN(910-FILE-NOTOPEN)        
"                                      NOTFND(910-FILE-NOTFND)          
"                                      ENDFILE(910-FILE-NOTFND)         
"                                      ERROR(910-FILE-OTHER-ERROR)      
"                      END-EXEC.                                        
"          MOVE        LNM-CUST-ID        TO      PB-H-ID-NO.           
090701     MOVE        LNM-AGENT-BR       TO      PB-H-BR-CODE.         
"          EXEC  CICS  READ  DATASET('PBHOUSE')                         
"                            INTO(PB-H-REC)                             
"                            LENGTH(PB-H-LEN)                           
"                            RIDFLD(PB-H-KEY)                           
"                            UPDATE                                     
"                      END-EXEC.                                        
060113*---------------設定：１建構／２修繕                            
"          IF        ( LNP-LN-CODE  =  79 )                             
"          THEN                                                         
"                      MOVE    01                TO   II                
"          ELSE                                                         
"                      MOVE    02                TO   II.               
      *****ENDIF                                                        
      *---------------設定：交易日                                    
961206     MOVE        TWA-TXN-YY7          TO   WK-TXN-YYYY.           
"          MOVE        TWA-TXN-MM7          TO   WK-TXN-MM.             
"          MOVE        TWA-TXN-DD7          TO   WK-TXN-DD.             
"          COMPUTE     WK-TXN-YYYY  =  WK-TXN-YYYY + 1911.              
"          MOVE        WK-TXN-DATE          TO   PB-H-L-TXN-DATE(II).   
110926 9184A-EXIT.                                                      
"            EXIT.                                                      
------*--------------處理：活力貸控管檔ＰＢＨＯＵＳＥ（沖正）---------
"      9185-UPD-TOT-FIL-RTN.                                            
110926*---------------讀ＰＢＨＯＵＳＥ檔                              
"          PERFORM     9184A-PBHOUSE-READ-RTN    THRU  9184A-EXIT.      
090701*---------------計算：現欠餘額、可用額度／合理性檢核            
"          COMPUTE     PB-H-LNL-AMT(II) =   PB-H-LNL-AMT(II)            
"                                         + CTF-RECV-AMT.               
"          COMPUTE     PB-H-PB-TREATY-AMT(II) =  PB-H-PB-TREATY-AMT(II) 
"                                              - CTF-RECV-AMT.          
"          PERFORM     91811-CHECK-TOT-RNT       THRU  91811-EXIT.      
090701*---------------更新檔案                                        
"          EXEC        CICS  REWRITE   DATASET('PBHOUSE')               
"                                      FROM(PB-H-REC)                   
"                                      LENGTH(PB-H-LEN)                 
"                      END-EXEC.                                        
"      9185-EXIT.                                                       
"           EXIT.                                                       
------*-----------------------------------------------------------------
       9186-INIT-LOG-RTN.              COPY LNK186.                     
840913 9210-CONVER-APRV-RTN.           COPY LNK210.                     
       9210-EXIT.  EXIT.                                                
980506 9220-RVBL-INT-PROC-RTN.         COPY LNK220N.                    
------*-----------------------------------------------------------------
891019 9251-READ-RATE.                                                  
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN(9251-CASE-NOTOPEN)                       
"                      NOTFND(9251-CASE-NOTFND)                         
"                      ERROR(9251-CASE-ERROR)                           
"                      END-EXEC.                                        
           MOVE TWA-M-VSAM-NORMAL   TO      TWA-VSAM-RETURN-CODE.       
"          MOVE        CTF-ACC-NO          TO      ALNRAT-ACC-NO.       
"          MOVE        CTF-LN-ID-NO        TO      ALNRAT-LN-ID-NO.     
"          EXEC  CICS  READ  DATASET('ALNRATE')                         
"                            INTO(ALNRAT)                               
"                            LENGTH(RATE-LEN)                           
"                            RIDFLD(ALNRAT-KEY)                         
"                      END-EXEC.                                        
            GO  TO   9251-EXIT.                                         
------*-----------------------------------------------------------------
       9251-CASE-NOTOPEN.                                               
"             MOVE      'D205'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
       9251-CASE-NOTFND.                                                
           MOVE        TWA-M-VSAM-NOTFND   TO      TWA-VSAM-RETURN-CODE.
           GO  TO      9251-EXIT.                                       
       9251-CASE-ERROR.                                                 
"             MOVE      'D205'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
       9251-EXIT.                                                       
            EXIT.                                                       
------*---------------更新優惠利率期限檔ALNRATE ----------------------
       9252-UPDE-RATE.                                                  
"          EXEC CICS   HANDLE CONDITION                                 
"                      NOTOPEN(9252-CASE-NOTOPEN)                       
"                      NOTFND(9252-CASE-NOTFND)                         
"                      ERROR(9252-CASE-ERROR)                           
"                      END-EXEC.                                        
           MOVE TWA-M-VSAM-NORMAL   TO      TWA-VSAM-RETURN-CODE.       
"          MOVE        CTF-ACC-NO          TO      ALNRAT-ACC-NO.       
"          MOVE        CTF-LN-ID-NO        TO      ALNRAT-LN-ID-NO.     
961206     MOVE        TWA-TXN-DAT7        TO      WKS-COMP-DATE.       
"          EXEC  CICS  READ  DATASET('ALNRATE')                         
"                            INTO(ALNRAT)                               
"                            LENGTH(RATE-LEN)                           
"                            RIDFLD(ALNRAT-KEY)                         
"                            UPDATE                                     
"                      END-EXEC.                                        
           IF        ( TWA-C-TXN-NOT-REVERSE )                          
           THEN                                                         
             IF        LN-RATE-NORMAL                                   
             THEN                                                       
                       MOVE    9                 TO   ALNRAT-TRINF-CODE 
961206                 MOVE    TWA-TXN-DAT7      TO   ALNRAT-L-TXN-DATE 
930518       ELSE                                                       
"                      NEXT  SENTENCE                                   
           ELSE                                                         
             IF      ( WKS-COMP-DATE       =  ALNRAT-L-TXN-DATE )       
970609           AND ( CTF-INT-BGN-DATE-N-R  <  WKS-END-RATE-DATE )     
"                AND ( LN-RATE-DELETE )                                 
             THEN                                                       
                       MOVE    0                 TO   ALNRAT-TRINF-CODE.
      *    ENDIF                                                        
"          EXEC  CICS  REWRITE    DATASET('ALNRATE')                    
"                                 FROM(ALNRAT)                          
"                                 LENGTH(RATE-LEN)                      
"                END-EXEC.                                              
"          EXEC  CICS  UNLOCK     DATASET('ALNRATE')  END-EXEC.         
           GO  TO      9252-EXIT.                                       
------*-----------------------------------------------------------------
       9252-CASE-NOTOPEN.                                               
"             MOVE      'D205'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
           GO  TO      9252-EXIT.                                       
       9252-CASE-NOTFND.                                                
           MOVE        TWA-M-VSAM-NOTFND   TO      TWA-VSAM-RETURN-CODE.
           GO  TO      9252-EXIT.                                       
       9252-CASE-ERROR.                                                 
"             MOVE      'D205'              TO      MSG-P-OUT-CODE.     
"            PERFORM   9146-LN-ERR-MSG-OUT-RTN.                         
       9252-EXIT.                                                       
            EXIT.                                                       
------*-----------------------------------------------------------------
031013 9267-ECHR-AMT-CAL-RTN.          COPY LNK267.                     
       935-UCPWRITE-HDL-RTN.           COPY PBK35.                      
------*---------------【&&&&】通知端末顯示主管確認畫後重啟交易------
040107 9351-SUP-RESTART-RTN.                                            
050614*---------------網銀：轉換錯誤訊息                              
"          IF        ( TWA-BR-CODE   =  '026'   )                       
"          THEN                                                         
"                      MOVE    'M6B2'            TO   MSG-P-OUT-CODE    
"                      PERFORM 9146-LN-ERR-MSG-OUT-RTN.                 
      *****ENDIF                                                        
040317*---------------錯誤訊息顯示                                    
"          MOVE        'T282'               TO   MSG-P-OUT-CODE.        
"          MOVE        208                  TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-DISPLAY        TO   MSG-P-OUT-TM-TYPE.     
"          CALL        'UCPWRITE'   USING   TWA  MSG-P-OUT.             
040317*---------------送&&&&識別碼ＴＯ端末                          
040107     MOVE        SPACES               TO   WS-MSG-DATA-JOURNAL.   
"          MOVE        83                   TO   MSG-P-LENGTH.          
"          MOVE        MSG-M-JOURNAL        TO   MSG-P-OUT-TM-TYPE.     
"          MOVE        'T6J2'               TO   MSG-P-OUT-CODE.        
"          MOVE        '&&&&'               TO   MSG-P-OUT-MSG.         
"          PERFORM     935-UCPWRITE-HDL-RTN                             
"          EXEC        CICS  RETURN    END-EXEC.                        
"      9351-EXIT.                                                       
"           EXIT.                                                       
------*---------------聯結ＰＢ存款程式--------------------------------
050122 9401-PB-LINK-RTN.                                                
"     *-----------------------------------------------------------------
"     *【留存】ＣＴＦ資料，因連動ＰＢ程式後ＣＴＦ資料會被修改，       
"     *                    為避免影響後續程式正確性，留存後再還原     
"     *-----------------------------------------------------------------
"          MOVE        CTF                  TO   WCTF-AREA.             
050122*---------------聯結ＰＢ程式                                    
"          EXEC        CICS  LINK PROGRAM ('PBLTRLNI')                  
"                                 COMMAREA(LPB-ACC-WORK-AREA)           
050614                            LENGTH  (123)                         
"                      END-EXEC.                                        
050122*--------------【還原】ＣＴＦ資料                               
"          MOVE        WCTF-AREA            TO   CTF.                   
      *---------------回覆訊息處理                                    
040317     IF        ( LPB-C-NORMAL-RC )                                
"                      GO  TO  9401-EXIT                                
"          ELSE IF   ( LPB-RC-CODE  =  'M019' )                         
"                      MOVE    'D181'            TO   MSG-P-OUT-CODE    
"          ELSE IF   ( LPB-RC-CODE  =  'M194' )                         
"                      MOVE    'D184'            TO   MSG-P-OUT-CODE    
"          ELSE IF   ( LPB-RC-CODE  NOT <  'D   ' )                     
"                AND ( LPB-RC-CODE  NOT >  'D999' )                     
"                      MOVE    LPB-RC-CODE       TO   MSG-P-OUT-CODE    
"          ELSE                                                         
"                      MOVE    'D620'            TO   MSG-P-OUT-CODE.   
      *****ENDIF                                                        
"          PERFORM     9146-LN-ERR-MSG-OUT-RTN.                         
"      9401-EXIT.                                                       
"           EXIT.                                                       
------*-----------------------------------------------------------------
871031 963-LINK-ACLACR-RTN.                                             
           EXEC    CICS    LINK    PROGRAM('ACLACR')                    
                                   COMMAREA(ACRF-P)                     
000225                             LENGTH (61)                          
                                   END-EXEC.                            
       963-EXIT.  EXIT.                                                 
991203 9630-LINK-ACLACR-RTN.                                            
"          EXEC    CICS    LINK    PROGRAM('ACLACR2')                   
"                                  COMMAREA(ACRF-P)                     
000131                             LENGTH (91)                          
"                                  END-EXEC.                            
991203 9630-EXIT.  EXIT.                                                
------*-----------------------------------------------------------------
871031 964-RETURN-HAND-RTN.                                             
           IF   ACRF-RETURN-CODE       NOT   =  SPACE                   
                MOVE ACRF-RETURN-CODE  TO   MSG-P-OUT-CODE              
                PERFORM   917-ERR-MSG-OUT-RTN.                          
       964-EXIT.  EXIT.                                                 
------*-----------------------------------------------------------------
810821 977-TIF-HDL-RTN.      COPY LNKT02.                               
991203                       COPY LNKEIR1.                              
------*----------------ＬＩＮＫ計息模組-------------------------------
110926 978-NACINT7-DATE-RTN.                                            
"          EXEC        CICS    LINK    PROGRAM ('NACINT7')              
"                                      COMMAREA(INT-GROUP)              
"                                      LENGTH  (INT-LENGTH)             
"                      END-EXEC.                                        
"          PERFORM     9164-LN-DATE-ERR-HDL-RTN.                        
"      978-EXIT.                                                        
"          EXIT.                                                        
------*-----------------------------------------------------------------
050614 991-GET-EIBDATE-RTN.            COPY ISK91.                      
------*---------------不正常結束--------------------------------------
       ABNORMAL-END-CONTROL.                                            
040317     MOVE        '### TXN-ERROR-END:' TO   DISP-ERR-MARK1.        
           MOVE        TWA-TXN-ID-CODE      TO   DISP-TXN-ID-CODE.      
           MOVE        TWA-MAP-PROG-NAME    TO   DISP-PROG-NAME.        
           MOVE        TWA-TM-ID-NO         TO   DISP-TM-ID-NO.         
           MOVE        TWA-TXN-TIME         TO   DISP-TXN-TIME.         
           EXEC  CICS  WRITE OPERATOR  TEXT(DISP-ERROR-END)   END-EXEC. 
           EXEC        CICS  RETURN    END-EXEC.                        
------*------------<<  END  OF  PROGRAM  >>-----------------------------