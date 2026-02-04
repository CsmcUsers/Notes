     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')

     FCRYPTOFM  CF   E             WORKSTN

      /COPY QSYSINC/QRPGLESRC,QC3CCI

      * Define C functions for Hex/Binary conversion
     D cvtch           PR                  EXTPROC('cvtch')
     D  target                         *   VALUE
     D  source                         *   VALUE
     D  length                       10I 0 VALUE

     D cvthc           PR                  EXTPROC('cvthc')
     D  target                         *   VALUE
     D  source                         *   VALUE
     D  length                       10I 0 VALUE

      * Define IBM i Encryption API
     D Qc3EncryptData  PR                  EXTPROC('Qc3EncryptData')
     D  clearData                      *   VALUE
     D  clearLen                     10I 0 CONST
     D  clearFmt                      8A   CONST
     D  algDesc                        *   VALUE
     D  keyDesc                        *   VALUE
     D  cryptProvider                 1A   CONST
     D  cryptDev                     10A   CONST
     D  encryptedData                  *   VALUE
     D  encryptedBufL                10I 0 CONST
     D  encryptedLen                 10I 0
     D  errorCode                          LIKE(QUSEC)

      * Algorithm Description (TDEA, ECB Mode, No Padding)
     D algDesc         DS
     D  algID                        10I 0 INZ(2)      // 2 = TDEA (Triple DES)
     D  blockLen                     10I 0 INZ(8)      // Block Size 8
     D  mode                          1A   INZ('0')    // 0 = ECB
     D  padOption                     1A   INZ('0')    // 0 = No Pad
     D  padChar                       1A   INZ(x'00')
     D  reserved                      5A   INZ(*ALLx'00')

      * Key Description (TDEA Key)
     D keyDesc         DS
     D  keyType                      10I 0 INZ(2)      // 2 = TDEA Key
     D  keyLen                       10I 0 INZ(16)     // 16 Bytes (Double Length)
     D  keyFmt                        1A   INZ('0')    // 0 = Binary
     D  reserved2                     3A   INZ(*ALLx'00')
     D  keyPtr                         *   INZ(%ADDR(fullKey))

     D fullKey         S             16A               // 16-byte Key for API
     D binKey          S              8A               // 8-byte Binary Key from input
     D binData         S              8A               // 8-byte Binary Data from input
     D binResult       S              8A               // 8-byte Binary Result

     D clearLen        S             10I 0 INZ(8)
     D encBufLen       S             10I 0 INZ(8)
     D encLen          S             10I 0
     D QUSEC           DS
     D  QUSBPRV                      10I 0 INZ(%SIZE(QUSEC))
     D  QUSBAVL                      10I 0 INZ(0)
     D  QUSEI                         7A
     D  QUSERVED                      1A

      * Main Logic
      /FREE
       DoU *IN03;
         Exfmt SCREEN1;
         If *IN03;
           Leave;
         EndIf;

         // 1. Convert Hex Key (16 chars) to Binary (8 Bytes)
         //    3DES needs 16 or 24 bytes. We repeat the 8-byte key (K1=K2).
         cvtch(%ADDR(binKey) : %ADDR(S_KEY) : 16);
         fullKey = binKey + binKey;

         // 2. Convert Hex Data (16 chars) to Binary (8 Bytes)
         cvtch(%ADDR(binData) : %ADDR(S_DATA) : 16);

         // 3. Encrypt using IBM i Cryptographic Services APIs
         Qc3EncryptData( %ADDR(binData)
                       : clearLen
                       : 'DATA0100'
                       : %ADDR(algDesc)
                       : %ADDR(keyDesc)
                       : '0'           // 0 = Any cryptographic provider
                       : *BLANKS
                       : %ADDR(binResult)
                       : encBufLen
                       : encLen
                       : QUSEC
                       );

         // 4. Handle error or convert result
         If QUSBAVL > 0;
           S_RESULT = 'ERROR: ' + QUSEI;
         Else;
           // Convert Binary Result to Hex for display
           cvthc(%ADDR(S_RESULT) : %ADDR(binResult) : 16);
         EndIf;

       EndDo;

       *INLR = *ON;
      /END-FREE
