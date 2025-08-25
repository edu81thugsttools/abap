# abap
ALV

REPORT Z_PROVAITEAM_DU23.


**********************************************************************
*21/11/2023 user:DU23 PROVA ITEAM 2023
**********************************************************************

* PROGRAMA : Z_PROVAITEAM_DU23
* OBJETIVO : Elaborar um programa ABAP para a geração de um relatório ALV onde sejam listadas todas as ‘Partidas em Aberto’ de Clientes e Fornecedores.

********************************************************************




 INCLUDE z_provaiteam_scr.
 INCLUDE Z_provaiteam_top.
 INCLUDE z_provaiteam_eve.
 INCLUDE Z_provaiteam_f01.


 ******************************************************************************



*&---------------------------------------------------------------------*
*&  INCLUDE           Z_PROVAITEAM_SCR
*&---------------------------------------------------------------------*


**********************************************************************
*21/11/2023 USER:DU23 PROVA ITEAM 2023
**********************************************************************

* PROGRAMA : Z_PROVAITEAM_DU23
* OBJETIVO : ELABORAR UM PROGRAMA ABAP PARA A GERAÇÃO DE UM RELATÓRIO ALV ONDE SEJAM LISTADAS TODAS AS ‘PARTIDAS EM ABERTO’ DE CLIENTES E FORNECEDORES.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

TABLES: BKPF. "(Cabeçalho do documento contábil)


*P_BUKRS EMPRESA BKPF-BUKRS ‘X’ ‘3000’

*S_BELNR N. DOCUMENTO BKPF-BELNR ‘X‘

*P_GJAHR ANO BKPF-GJAHR ‘X’ ‘2008’

*P_KUNNR CLIENTE RADIOBUTTON ‘X’

*P_LIFNR FORNECEDOR RADIOBUTTON ‘X’

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS  P_BUKRS TYPE BKPF-BUKRS DEFAULT '3000'.  "(BKPF-BUKRS)  CLIENTE
SELECT-OPTIONS: S_BELNR FOR BKPF-BELNR.              "(BKPF-BELNR) FORNCEDOR

PARAMETERS: P_GJAHR TYPE BKPF-GJAHR DEFAULT '2008',  "(BKPF-GJAHR)
            P_KUNNR RADIOBUTTON GROUP RB1,           "RADIOBUTTON "CLIENTE
            P_LIFNR RADIOBUTTON GROUP RB1.           "RADIOBUTTON " FORNECEDOR
SELECTION-SCREEN END OF BLOCK B1.
**********************************************************************



*&---------------------------------------------------------------------*
*&  INCLUDE           Z_PROVAITEAM_TOP
*&---------------------------------------------------------------------*


**********************************************************************
*21/11/2023 USER:DU23 PROVA ITEAM 2023
**********************************************************************
* PROJETO  :
* PROGRAMA : Z_PROVAITEAM_DU23
* TRANSAÇÃO:
* DESCRIÇÃO:
* OBJETIVO : ELABORAR UM PROGRAMA ABAP PARA A GERAÇÃO DE UM RELATÓRIO ALV ONDE SEJAM LISTADAS TODAS AS ‘PARTIDAS EM ABERTO’ DE CLIENTES E FORNECEDORES.

********************************************************************

*&---------------------------------------------------------------------*
*& TABELAS STANDARD
*&---------------------------------------------------------------------*

"(04-TAB)
******************
TABLES: " BKPF,"-(CABEÇALHO DO DOCUMENTO CONTÁBIL)
         BSEG,"-(SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA)
         BSID,"-(CONTABILIDADE: ÍNDICE SECUNDÁRIO PARA CLIENTES)
         BSIK."-(CONTABILIDADE: ÍNDICE SECUNDÁRIO PARA FORNECEDORES)
*
******************


"01-SELEÇÃO DE TYPES

"01-DECLARAÇÃO DE TIPOS INTERNOS

*&---------------------------------------------------------------------*
*& TYPES.
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF TY_BKPF, "-----------------------(BKPF)  "  " "  "
    BUKRS TYPE BKPF-BUKRS,  "BUKRS  = P_BUKRS                 "
    BELNR TYPE BKPF-BELNR,  "BELNR  = S_BELNR                  "
    GJAHR TYPE BKPF-GJAHR,  "GJAHR  = P_GJAHR                   "
  END OF TY_BKPF,                                                 "
                                                                    "
                                                                     "
                                                                      "
                                                                         "
  BEGIN OF TY_BSEG, "---------------------(BSEG)  "                        "
    BUKRS TYPE BSEG-BUKRS,                           "                       "
    BELNR TYPE BSEG-BELNR,                             "                        "
    GJAHR TYPE BSEG-GJAHR,                               "                        "
    BUZEI TYPE BSEG-BUZEI,                                 "                        "
    KUNNR TYPE BSEG-KUNNR,                                   """"""""""""""""""""""[BSEG]
    LIFNR TYPE BSEG-LIFNR,                                                       "   "
  END OF TY_BSEG,                                                              "    "
                                                                            "      "
                                                                          "       "
                                                                        "        "
  BEGIN OF TY_BSID, "----------------------(BSID)  "  "  "   "  "   "           "
    BUKRS TYPE BSID-BUKRS,                                                     "
    KUNNR TYPE BSID-KUNNR,                                                    "
    GJAHR TYPE BSID-GJAHR,                                                   "
    BELNR TYPE BSID-BELNR,                                                  "
    BUZEI TYPE BSID-BUZEI,                                                 "
    BUDAT TYPE BSID-BUDAT,                                                "
    WAERS TYPE BSID-WAERS,                                               "
    BLART TYPE BSID-BLART,                                              "
    MONAT TYPE BSID-MONAT,                                             "
    BSCHL TYPE BSID-BSCHL,                                            "
    DMBTR TYPE BSID-DMBTR,                                           "
  END OF TY_BSID,                                                   "
                                                                   "
                                                                  "
                                                                 "
  BEGIN OF TY_BSIK, "----------------------(BSIK)" " " " " " "
    BUKRS TYPE BSIK-BUKRS,
    LIFNR TYPE BSIK-LIFNR,
    GJAHR TYPE BSIK-GJAHR,
    BELNR TYPE BSIK-BELNR,
    BUZEI TYPE BSIK-BUZEI,
    BUDAT TYPE BSIK-BUDAT,
    WAERS TYPE BSIK-WAERS,
    BLART TYPE BSIK-BLART,
    MONAT TYPE BSIK-MONAT,
    BSCHL TYPE BSIK-BSCHL,
    DMBTR TYPE BSIK-DMBTR,
  END OF TY_BSIK,


  "_______//_______


  ""3.1.2	– CASO O RADIOBUTTON CLIENTE ESTEJA MARCADO,
  "SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA BSID (CONTABILIDADE: ÍNDICE SECUNDÁRIO PARA CLIENTES)


  ""3.1.4	– GERAR UMA TABELA INTERNA T_SAIDA COM OS CAMPOS A SEREM EXIBIDOS NO ALV.


  BEGIN OF TY_SAIDA, "-----------------------(T_SAIDA)
    MARK  TYPE C,
    BUKRS TYPE BSID-BUKRS,  "EMPRESA
    KUNNR TYPE BSID-KUNNR,  "Nº CLIENTE -------------------------------------CLIENTE
    LIFNR TYPE BSIK-LIFNR,  "Nº DO FORNECEDOR ------------------------------FORNECERDOR
    GJAHR TYPE BSID-GJAHR,  "EXERCÍCIO
    BELNR TYPE BSID-BELNR,  "Nº DOCUMENTO DE UM DOCUMENTO CONTÁBIL
    BUZEI TYPE BSID-BUZEI,  "Nº LINHA DE LANÇAMENTO NO DOCUMENTO CONTÁBIL
    BLART TYPE BSID-BLART,  "DATA NO DOCUMENTO
    BUDAT TYPE BSID-BUDAT,  "DATA DE LANÇAMENTO NO DOCUMENTO
    WAERS TYPE BSID-WAERS,  "CÓDIGO DA MOEDA
    MONAT TYPE BSID-MONAT,  "MÊS DO EXERCÍCIO
    BSCHL TYPE BSID-BSCHL,  "CHAVE DE LANÇAMENTO
    DMBTR TYPE BSID-DMBTR,  "MONTANTE EM MOEDA INTERNA
*    VL_CLIENTE TYPE STRING,
*    VL_FORNECEDOR TYPE STRING,
*    SAIDA TYPE CHAR100,     " DESCRICAO TYPE CHAR100,
  END OF TY_SAIDA.
*
*    BEGIN OF T_CSV,
*    ONE TYPE CHAR600,
*  END OF T_CSV.
  "________________________

*&---------------------------------------------------------------------*
*& TAB_INTERNAS
*&---------------------------------------------------------------------*

DATA: T_BKPF       TYPE TABLE OF TY_BKPF,
      T_BSEG       TYPE TABLE OF TY_BSEG,
      T_BSID       TYPE TABLE OF TY_BSID,
      T_BSIK       TYPE TABLE OF TY_BSIK,
      T_LISTHEADER TYPE TABLE OF  SLIS_LISTHEADER,     " CABEÇALHO
      T_FIELDCAT   TYPE TABLE OF  SLIS_FIELDCAT_ALV,   " CATÁLOGO DE CAMPOS
      T_SORT       TYPE TABLE OF  SLIS_SORTINFO_ALV,   " ORDENAÇÃO, QUEBRAS

      T_ALV_AUX  TYPE TABLE OF STRING,
      T_ALV_TXT TYPE TABLE OF STRING,
      T_ALV_CSV TYPE TABLE OF STRING,
      T_SAIDA      TYPE TABLE OF TY_SAIDA.



DATA: W_BKPF       TYPE TY_BKPF,
      W_BSEG       TYPE TY_BSEG,
      W_BSID       TYPE TY_BSID,
      W_BSIK       TYPE TY_BSIK,
      W_FIELDCAT   TYPE SLIS_FIELDCAT_ALV, " CATÁLOGO DE CAMPOS
      W_LAYOUT     TYPE SLIS_LAYOUT_ALV, " LAYOUT PARA SAÍDA
      W_LISTHEADER TYPE SLIS_LISTHEADER, " CABEÇALHO
      W_SORT       TYPE SLIS_SORTINFO_ALV, " ORDENAÇÃO, QUEBRAS
      W_ALV_AUX    TYPE STRING,
      W_ALV_TXT    TYPE STRING,
      W_CSV_ALV    TYPE STRING,
      W_SAIDA      TYPE TY_SAIDA.     "TAB DE SAIDA


      CONSTANTS: c_cliente_txt    TYPE string VALUE 'C:\USERS\DUGAB\DESKTOP\PARTIDAS\PARTIDAS DE CLIENTES.txt',
      c_cliente_csv    TYPE string VALUE 'C:\USERS\DUGAB\DESKTOP\PARTIDAS\PARTIDAS DE CLIENTES.CSV',
      c_fornecedor_txt TYPE string VALUE 'C:\USERS\DUGAB\DESKTOP\PARTIDAS\PARTIDAS DE FORNECEDOR.txt',
      c_fornecedor_csv TYPE string VALUE 'C:\USERS\DUGAB\DESKTOP\PARTIDAS\PARTIDAS DE FORNECEDOR.CSV'.

      *******************************************************************************

      *&---------------------------------------------------------------------*
*&  INCLUDE           Z_PROVAITEAM_EVE
*&---------------------------------------------------------------------*

**********************************************************************
*21/11/2023 USER:DU23 PROVA ITEAM 2023
**********************************************************************
* PROJETO  :
* PROGRAMA : Z_PROVAITEAM_DU23
* TRANSAÇÃO:
* DESCRIÇÃO:
* OBJETIVO : ELABORAR UM PROGRAMA ABAP PARA A GERAÇÃO DE UM RELATÓRIO ALV ONDE SEJAM LISTADAS TODAS AS ‘PARTIDAS EM ABERTO’ DE CLIENTES E FORNECEDORES.

********************************************************************

"LOGICA DO PROGRAMA.

START-OF-SELECTION.

  PERFORM z_seleciona_dados. "----------01

END-OF-SELECTION.
  PERFORM z_processa_dados.  "----------02
  IF  t_saida[] IS NOT INITIAL.

    PERFORM z_montar_alv.      "----------03
    PERFORM f_sort.
    PERFORM z_monta_cabecalho.
    PERFORM z_exibir_alv.      "----------04
*    PERFORM zf_status.
  ENDIF.


  ********************************************************************************************


  *&---------------------------------------------------------------------*
*&  INCLUDE           Z_PROVAITEAM_F01
*&---------------------------------------------------------------------*

FORM z_seleciona_dados .
*
*    "ESTE TRECHO DE CÓDIGO ESTÁ FAZENDO UMA VERIFICAÇÃO PARA GARANTIR QUE PELO MENOS UM DOS PARÂMETROS P_KUNNR OU P_LIFNR FOI INFORMADO ANTES DE CONTINUAR A EXECUÇÃO.
*
*    ***********************************************************************
*    *  IF P_KUNNR IS INITIAL AND P_LIFNR IS INITIAL.
*    *    MESSAGE 'INFORME PELO MENOS UM TIPO DE REGISTRO' TYPE 'E'.
*    *    EXIT.
*    *  ENDIF.
*    ***********************************************************************
*
*    *******************************************
*    *P_KUNNR    CLIENTE     RADIOBUTTON   ‘X’ *
*    *P_LIFNR    FORNECEDOR  RADIOBUTTON   ‘X’ *
*    *******************************************
*
*
*    *--------------->>>> A LÓGICA É:
*    *
*    *VERIFICAR SE P_KUNNR E P_LIFNR ESTÃO VAZIOS
*    *SE SIM, EXIBIR MENSAGEM DE ERRO INFORMANDO QUE PELO MENOS UM PRECISA SER PREENCHIDO
*    *SAIR DA EXECUÇÃO PARA NÃO CONTINUAR COM PARÂMETROS INVÁLIDOS
*    *DESSA FORMA, O PROGRAMA GARANTE QUE O USUÁRIO INFORME PELO MENOS UM DOS PARÂMETROS ANTES DE CONTINUAR A EXECUÇÃO, EVITANDO ERROS.
*
*    ****************************************  OU  *************************
*
*    *  IF P_KUNNR = 'X'." CÓDIGO PARA LISTAR TODAS AS 'PARTIDAS EM ABERTO' DE CLIENTES
*    *    MESSAGE 'INFORME PELO MENOS UM TIPO DE PARCEIRO' TYPE 'E'.
*    *  ELSEIF P_LIFNR = 'X'." CÓDIGO PARA LISTAR TODAS AS 'PARTIDAS EM ABERTO' DE FORNECEDORES
*    *    MESSAGE 'INFORME PELO MENOS UM TIPO DE PARCEIRO' TYPE 'E'.
*    *  ENDIF.
*

*    "SELECT
*
*
*    *START-OF-SELECTION.
*    *  SELECT BUKRS BELNR GJAHR FROM BKPF INTO TABLE LT_BKPF
*    *    WHERE BUKRS = P_BUKRS
*    *      AND BELNR IN S_BELNR
*    *      AND GJAHR = P_GJAHR
*    *      AND ( ( P_KUNNR = 'X' AND BLART = 'DR' ) OR
*    *            ( P_LIFNR = 'X' AND BLART = 'KR' ) ).
*    *  LOOP AT LT_BKPF INTO LS_BKPF.
*    *    " CÓDIGO PARA PROCESSAR OS DADOS SELECIONADOS
*    *  ENDLOOP.
*
*
*    *****************************************************************************
  IF p_kunnr EQ abap_true.
    "[1ºPRIMEIRO SELECT]
    "SELEÇÃO DE DADOS DA ------------------(BKPF)
    SELECT bukrs
           belnr
           gjahr
     FROM bkpf
    INTO TABLE t_bkpf "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      WHERE bukrs EQ p_bukrs                                                   "
        AND belnr IN s_belnr                                                   "
        AND gjahr EQ p_gjahr                                                   "
        AND blart EQ 'DR'. "---------------cliente-------------------[DR]             "
    "
    IF sy-subrc NE 0.                                                       "
      FREE t_bkpf[].                                                         "
    ENDIF.                                                                  "
    "
  ELSE.                                                                      "
    "
    SELECT bukrs                                                               "
           belnr                                                               "
           gjahr                                                               "
     FROM bkpf                                                                 "
    INTO TABLE t_bkpf[]  "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
     WHERE bukrs EQ p_bukrs                                                    "
      AND  belnr IN s_belnr                                                    "
      AND  gjahr EQ p_gjahr                                                    "
      AND  blart EQ 'KR'. "----------------fornecedor-------------------[KR]             "
    "
    IF sy-subrc NE 0.                                                         "
      FREE t_bkpf[].                                                          "
    ENDIF.                                                                   "
  ENDIF.                                                                   "
  "
  "
  IF t_bkpf[] IS NOT INITIAL.                                       "
    "
    "-------------//---------------                              "
    "[2º SEGUNDO SELECT]                                       " " " " " " "COM FOR ALL ENTRIES NA (T_BKPF)
    "SELEÇÃO DE DADOS DA -------------------(BSEG)          "
    SELECT bukrs                                        "
           belnr                                    "
           gjahr                               "
           buzei                           "
           kunnr                        "
           lifnr                      "
      FROM bseg                     "
     INTO TABLE t_bseg[]           "   <<<<<<<<<<<<<<<<<<<<<<<<<<<"
      FOR ALL ENTRIES IN t_bkpf[] "                               "
     WHERE  bukrs EQ t_bkpf-bukrs                                 "
        AND belnr EQ t_bkpf-belnr                                 "
        AND gjahr EQ t_bkpf-gjahr.                                "
    "
    IF sy-subrc NE 0.                                           "
      FREE t_bseg[].                                            "
    ELSE.                                                        "
      "
      "
      IF p_kunnr EQ abap_true.                                   "
        "[3ºTERCEIRO SELECT] "------------------------(BSID)      "
        SELECT bukrs                                          "
               kunnr                                        "
               gjahr                                      "
               belnr                                   "
               buzei                                 "
               budat                               "
               waers                              "
               blart                             "
               monat                            "
               bschl                           "
               dmbtr                          "
        FROM bsid                           "
       INTO TABLE t_bsid[]               "
        FOR ALL ENTRIES IN t_bseg[]   "   COM FOR ALL ENTRIES NA (T_BSEG)
          WHERE bukrs EQ t_bseg-bukrs
            AND kunnr EQ t_bseg-kunnr
            AND gjahr EQ t_bseg-gjahr
            AND belnr EQ t_bseg-belnr
            AND buzei EQ t_bseg-buzei.

        IF sy-subrc NE 0.
          FREE t_bsid[].
        ENDIF.

      ELSE.


        "[4º QUARTO SELECT] "-----------------------(BSIK)

        SELECT bukrs
               lifnr
               gjahr
               belnr
               buzei
               budat
               waers
               blart
               monat
               bschl
               dmbtr
          FROM bsik
         INTO TABLE t_bsik[]
          FOR ALL ENTRIES IN t_bseg[]
        WHERE bukrs EQ t_bseg-bukrs
          AND lifnr EQ t_bseg-lifnr
          AND gjahr EQ t_bseg-gjahr
          AND belnr EQ t_bseg-belnr
          AND buzei EQ t_bseg-buzei.

        IF sy-subrc NE 0.
          FREE t_bsik[].
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************


*     REPORT Z_PROVAITEAM_DU23.
*----------------------------------------------------------------------*
*INCLUDE Z_PROVAITEAM_DU23_Z_PROCESSF01.
*----------------------------------------------------------------------*

**********************************************************************
*21/11/2023 USER:DU23 PROVA ITEAM 2023
**********************************************************************
* PROJETO  :
* PROGRAMA : Z_PROVAITEAM_DU23
* TRANSAÇÃO:
* DESCRIÇÃO:
* OBJETIVO : ELABORAR UM PROGRAMA ABAP PARA A GERAÇÃO DE UM RELATÓRIO ALV ONDE SEJAM LISTADAS TODAS AS ‘PARTIDAS EM ABERTO’ DE CLIENTES E FORNECEDORES.

********************************************************************


*&---------------------------------------------------------------------*
*&      FORM  Z_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*3  – PROCESSAMENTO DOS DADOS: "
*3  – PROCESSAMENTO DOS DADOS:   "
*3  – PROCESSAMENTO DOS DADOS:     "                     " >(3 CAMPOS APENAS )
*3  – PROCESSAMENTO DOS DADOS:      "                "
*3  – PROCESSAMENTO DOS DADOS:        "           "
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3 PARA CADA REGISTRO SELECIONADO NA TABELA (BSEG) (SEGMENTO DO DOCUMENTO CONTABILIDADE FINANCEIRA):
*3.1.1  – SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA (BKPF)
*3.1.1  – SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA (BKPF)
*3.1.1  – SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA (BKPF)
*                                                                "
"
FORM z_processa_dados .                                               "
  " >(3 CAMPOS APENAS )


  "ONDENAR A TABELA T_BKPF T_BSID T_BSIK.
  "   DEPOIS DE ORDENAR AS TABELAS, VOCÊ PODE USAR O COMANDO READ TABLE PARA BUSCAR REGISTROS ESPECÍFICOS DAS TABELAS.
  "    A ORDENAÇÃO DAS TABELAS MELHORA A EFICIÊNCIA DO COMANDO READ TABLE QUANDO USADO COM A ADIÇÃO BINARY SEARCH.


  SORT: t_bkpf BY bukrs     " ORDENAR A TABELA T_BKPF
                  belnr
                  gjahr,
        t_bsid BY bukrs     " ORDENAR A TABELA T_BSID
                  kunnr
                  belnr
                  buzei,
        t_bsik BY bukrs     " ORDENAR A TABELA T_BSIK
                  lifnr
                  gjahr
                  belnr
                  buzei.

  "          |   | """>>|    |
  "          |___|      |    |
  LOOP AT t_bseg INTO w_bseg.  "PARA CADA REGISTRO SELECIONADO NA TABELA ((((BSEG))))

    READ TABLE t_bkpf INTO w_bkpf WITH KEY
                                  bukrs = w_bseg-bukrs    " CAMPOS CHAVE
                                  belnr = w_bseg-belnr
                                  gjahr = w_bseg-gjahr BINARY SEARCH.  " SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA BKPF
    ">>>>>>>>>>   T_BKPF  PROCURANDO UMA ENTRADA COM AS CHAVES BUKRS, BELNR      & BELNR  GJAHR CORRESPONDENTES AOS VALORES DE W_BSEG.
    IF sy-subrc NE 0.                                    "|"   "|"                  "|"
      CONTINUE. "CONTINUA PARA O PROXIMO REGISTRO CORRESPONDENTE
    ELSE.

      "3.1.2  – CASO O RADIOBUTTON CLIENTE ESTEJA MARCADO, SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA (((BSID))) (CONTABILIDADE: ÍNDICE SECUNDÁRIO PARA CLIENTES).

      IF p_kunnr EQ abap_true. " ESTE COMANDO VERIFICA SE A VARIÁVEL P_KUNNR É IGUAL A ABAP_TRUE. ISSO PROVAVELMENTE ESTÁ RELACIONADO A UMA CONDIÇÃO BASEADA EM UM RADIOBUTTON CHAMADO "CLIENTE" QUE DEFINE SE APENAS REGISTROS RELACIONADOS A CLIENTES
        "DEVEM SER MOSTRADOS NO ALV.
        "IRA LER A TABELA T_BISID PROCURANDO  UMA ENTRADA COM AS CHAVES BUKRS, KUNNR, BELNR E BUZEI CORRESPONDENTES AOS VALORES DE W_BSEG.
        "ISSO É FEITO PARA VERIFICAR SE O REGISTRO EXISTE NA TABELA T_BSID E, ASSIM, FILTRAR OS REGISTROS QUE SERÃO MOSTRADOS NO ALV
        READ TABLE t_bsid INTO w_bsid WITH KEY
                                      bukrs = w_bseg-bukrs    " CAMOPOS CHAVE
                                      kunnr = w_bseg-kunnr
                                      belnr = w_bseg-belnr
                                      buzei = w_bseg-buzei BINARY SEARCH.

        "CASO O RADIOBUTTON CLIENTE ESTEJA MARCADO,
        IF sy-subrc EQ 0.  "-------------------------------------SE TIVER MARCADO   O RADIO BOTTON EM CLIENTE É  ESSE CAMINHO
          "SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA >>>>>>>>>>>>>>>>>>(((BSID)))<<<<<<<<<<<<<<<<<<<

          "3.1.4  – Gerar uma tabela interna T_SAIDA com os campos a serem exibidos no ALV.


          w_saida-bukrs = w_bsid-bukrs.
          w_saida-kunnr = w_bsid-kunnr.
          w_saida-gjahr = w_bsid-gjahr.
          w_saida-belnr = w_bsid-belnr.
          w_saida-buzei = w_bsid-buzei.
          w_saida-blart = w_bsid-blart.
          w_saida-budat = w_bsid-budat.
          w_saida-waers = w_bsid-waers.
          w_saida-monat = w_bsid-monat.
          w_saida-bschl = w_bsid-bschl.
          w_saida-dmbtr = w_bsid-dmbtr.


          APPEND w_saida TO t_saida[].
          FREE w_saida.

        ENDIF.


      ELSE. "------------------------------------------------SE TIVER MARCADO   O RADIO BOTTON EM FORNECEDOR É ESSE CAMINHO
        "3.1.3  –  "CASO O RADIOBUTTON CLIENTE ESTEJA MARCADO,
        "SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA >>>>>>>>>>>>>(((BSIK)))<<<<<<<<<<<<<<

        READ TABLE t_bsik INTO w_bsik WITH KEY bukrs = w_bseg-bukrs
                                                lifnr = w_bseg-lifnr   " CAMPOS CHAVE
                                                gjahr = w_bseg-gjahr
                                                belnr = w_bseg-belnr
                                                buzei = w_bseg-buzei.
        IF sy-subrc EQ 0.
          "3.1.4  – Gerar uma tabela interna T_SAIDA com os campos a serem exibidos no ALV.
          w_saida-bukrs = w_bsik-bukrs.
          w_saida-lifnr = w_bsik-lifnr.
          w_saida-gjahr = w_bsik-gjahr.
          w_saida-belnr = w_bsik-belnr.
          w_saida-buzei = w_bsik-buzei.
          w_saida-blart = w_bsik-blart.
          w_saida-budat = w_bsik-budat.
          w_saida-waers = w_bsik-waers.
          w_saida-monat = w_bsik-monat.
          w_saida-bschl = w_bsik-bschl.
          w_saida-dmbtr = w_bsik-dmbtr.

          APPEND w_saida TO t_saida[].
          FREE w_saida.

        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
*********************A********L**********V****************************
**********************************************************************
**********************************************************************
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:
*4  - CAMPOS A SEREM EXIBIDOS NO LAYOUT DO RELATÓRIO ALV:

*&---------------------------------------------------------------------*
*&                  FORM  Z_MONTAR_ALV
*&---------------------------------------------------------------------*
* SAO AS INFORMAÇOES QUE COMPOEM OS CAMPO  INFORMAÇOES DO CAMPO  CARACTERISTICA DE CADA CAMPO. TIPO NUMERO , CAMPO EDITAVEL, TAMANHA DE 10 POSIÇÕES.
"COLOCANDO AS INFORMAÇÕES DENTRO DO MEU RELATORIO DENTRO DE CADA COLUNA DO RELATÓRIO PREENCHENDO AS INFORMAÇÃO PARA EXIBIR A ESTRUTURA DE TABELA RELATORIAL .

* DEFININDO O LAYOUT DO RELATÓRIO ALV
FORM z_montar_alv.  "MONTAR FIELDCAT

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'MARK'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'SELECIONE'.
  w_fieldcat-checkbox  = abap_true.  "flag
  w_fieldcat-edit = abap_true.       "clicavel
  APPEND w_fieldcat TO t_fieldcat.


  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUKRS'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'EMPRESA'.
  APPEND w_fieldcat TO t_fieldcat.

  "5.1-Efetuar quebra pelos campos Empresa, Cliente (se o radiobutton Cliente estiver marcado) ou (se o radiobutton fornecedor estiver marcado).
**********************************************************************
  IF p_kunnr EQ abap_true.  " com a condição de IF  para os (RADIOBUTTONS)

    CLEAR w_fieldcat.
    w_fieldcat-fieldname = 'KUNNR'.  " CASO O RADIOBUTTON CLIENTE ESTEJA MARCADO, SÓ MOSTRAR NO ALV REGISTROS  (((((( BKPF))))))
    w_fieldcat-tabname   = 'T_SAIDA'.
    w_fieldcat-seltext_l = 'CLIENTE'.
    APPEND w_fieldcat TO t_fieldcat.

  ELSE.

    CLEAR w_fieldcat.
    w_fieldcat-fieldname = 'LIFNR'.    "CASO O RADIOBUTTON FORNECEDOR ESTEJA MARCADO, SÓ MOSTRAR NO ALV REGISTROS QUE EXISTAM NA TABELA ((((((BSIK))))))
    w_fieldcat-tabname   = 'T_SAIDA'.
    w_fieldcat-seltext_l = 'FORNECEDOR'.
    APPEND w_fieldcat TO t_fieldcat.


  ENDIF.



  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'GJAHR'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'ANO'.
  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BELNR'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'N° DOCUMENTO'.
  w_fieldcat-hotspot = abap_true.      " CAMPO CLICAVEL
  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUZEI'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'N° LINHA'.
  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BLART'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'TIPO DE DOCUMENTO'.
  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUDAT'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'DATA DE LANÇAMENTO'.
  APPEND w_fieldcat TO t_fieldcat.

**********************************************************************
*5.3  – OS CAMPOS MOEDA, MÊS DO EXERCÍCIO E CHAVE DE LANÇAMENTO NÃO DEVERÃO APARECER NA EXIBIÇÃO INICIAL DO ALV.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'WAERS'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'MOEDA'. "----------------MOEDA
  w_fieldcat-no_out    = abap_true.
  APPEND w_fieldcat TO t_fieldcat.


  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'MONAT'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'MÊS DE EXERCÍCIO'."-----------MES DE EXERCÍCIO
  w_fieldcat-no_out    = abap_true.
  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BSCHL'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'CHAVE DE LANÇAMENTO'. "-------------CHAVE DE LANÇAMENTO
  w_fieldcat-no_out    = abap_true.
  APPEND w_fieldcat TO t_fieldcat.
**********************************************************************

  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'DMBTR'.
  w_fieldcat-tabname   = 'T_SAIDA'.
  w_fieldcat-seltext_l = 'MONTANTE'.
  w_fieldcat-do_sum    = abap_true. "5.2  – O CAMPO MONTANTE DEVERÁ POSSUIR SOMATÓRIA.
  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------
*&      FORM  Z_MONTA_CABEÇALHO
*&---------------------------------------------------------------------


"O CABEÇALHO DO RELATÓRIO, ALÉM DO TÍTULO (SE O RADIOBUTTON CLIENTE ESTIVER MARCADO ‘PARTIDAS EM ABERTO DE CLIENTES’
" OU SE O RADIOBUTTON FORNECEDOR ESTIVER MARCADO ‘PARTIDAS EM ABERTO DE FORNECEDORES’),
"DEVERÁ TER A DATA (SY-DATUM) E A HORA (SY-UZEIT) DE EXECUÇÃO DO MESMO.

FORM z_monta_cabecalho .
*DECLARAÇÃO DE VARIÁVEIS LOCAIS PRA MANIPULAR A DATA E HORA
  DATA: d             TYPE d,
        h             TYPE t,
        DATA(19)      TYPE c,   "DD/MM/AA (/8)
        hora(8)       TYPE c,   "HH:MM:SS(:8)
        timestamp(30) TYPE c.

  d = sy-datum.  " OBTER OS DADOS  ATUAIS DO SYSTEMA SA PARA DATA E HORA.
  h = sy-uzeit.  "  OBTER OS DADOS  ATUAIS DO SYSTEMA SAP PARAR DATA E HORA.

*FORMATAÇÃO DA DATA ATUAL EM UM FORMATO LEGÍVEL (DD/MM/AAAA) E ARMAZENAMENTO DO RESULTADO NA VARIÁVEL DATA.
*(2023/11/27)    = 8
  CONCATENATE D+6(2) " = 202311 (27)  = DE 8 NUMERO ELE FILTRA 2 SENDO ASSIM        27/
              D+4(2) " = 2023 (11)    = DE 6 NUMERO ELE FILTRA 2 SENDO ASSIM         /11
              D+0(4) " = (2023)     " FILTRA O Nº FINAL SENDO ASSIM O ANO              /2023
              INTO data      SEPARATED BY '/'.   " E SEPARA POR BARRA
*FORMATAÇÃO DA HORA ATUAL EM UM FORMATO LEGÍVEL (HH:MM:SS) E ARMAZENAMENTO DO RESULTADO NA VARIÁVEL HORA.
  CONCATENATE H+0(2)  " 10: HORAS
              H+2(2)  " 00: MINUTOS
              H+4(2) INTO hora      SEPARATED BY ':'. "SS SEGUNDO

  CONCATENATE data hora  INTO timestamp SEPARATED BY space.  " ESTA LINHA ESTÁ CONCATENANDO AS STRINGS DE DATA E HORA JUNTAS COM UM ESPAÇO COMO O SEPARADOR PARA FORMAR UM CARIMBO DE DATA/HORA.

  FREE: t_listheader[], w_listheader.
*ESTA LINHA ESTÁ LIBERANDO A MEMÓRIA OCUPADA PELA TABELA T_LISTHEADER E PELA ESTRUTURA W_LISTHEADER. ISSO É FEITO PARA GARANTIR QUE NÃO HAJA DADOS RESIDUAIS NA TABELA OU NA ESTRUTURA ANTES DE USÁ-LOS PARA CONSTRUIR O CABEÇALHO DO RELATÓRIO ALV.

*  SE O RADIOBUTTON CLIENTE ESTIVER MARCADO ‘PARTIDAS EM ABERTO DE CLIENTES’
  IF p_kunnr EQ abap_true.  " SEMPRE COM ESSA CONDIÇÃO

    w_listheader-typ  = 'H'. "FONTE
    w_listheader-info = 'PARTIDAS EM ABERTO DE CLIENTES'.
    APPEND w_listheader TO t_listheader.
    FREE w_listheader.
*    OU SE O RADIOBUTTON FORNECEDOR ESTIVER MARCADO ‘PARTIDAS EM ABERTO DE FORNECEDORES’),
  ELSE.
*    OU SE O RADIOBUTTON FORNECEDOR ESTIVER MARCADO ‘PARTIDAS EM ABERTO DE FORNECEDORES’),

    w_listheader-typ  = 'H'. "TIPO DA FONTE
    w_listheader-info = 'PARTIDAS EM ABERTO DE FORNECEDORES'.
    APPEND w_listheader TO t_listheader.
    FREE w_listheader.

  ENDIF.
*  É DEFINIDO COMO O CARIMBO DE DATA/HORA.    27/11/2023 10:27
  w_listheader-typ  = 'A'. "TIPO DA FONTE
  w_listheader-info = timestamp.
  APPEND w_listheader TO t_listheader.

ENDFORM.

*&---------------------------------------------------------------------
*&      FORM  F_TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM f_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE' " FUNÇÃOPADRÃO DO SAP PARA ESCREVER COMENTÁRIOS NO RELATÓRIO ALV.
    EXPORTING
      it_list_commentary = t_listheader.    "  É UMA TABELA QUE CONTÉM OS COMENTÁRIOS A SEREM ESCRITOS NO RELATÓRIO ALV. A TABELA T_LISTHEADER É PASSADA PARA ESTA FUNÇÃO, QUE CONTÉM OS COMENTÁRIOS QUE VOCÊ DEFINIU.

ENDFORM.

*&---------------------------------------------------------------------
*&      FORM  Z_EXIBE_ALV
**&---------------------------------------------------------------------
*EXIBIÇÃO DO ALV . RESPONSÁVEL POR EXIBIR OS DADOS NA FORMA DE UM RELATÓRIO ALV
FORM z_exibir_alv.

  w_layout-colwidth_optimize = abap_true."OTIMIZA LARGURA DA COLUNA.
  w_layout-zebra             = abap_true."ZEBRADO
*w_layout-box_fieldname     = 'MARK'. "-------------------------------------------------------<<<<<<<<<<<<<<<<<<<<<<<<<<


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid " NOME DO PROGRAMA
      is_layout                = w_layout
      it_fieldcat              = t_fieldcat
      it_sort                  = t_sort
      i_callback_pf_status_set = 'ZF_STATUS'   "STATUS GUI--» aqui sempre preciso por o nome do FORM onde esta meu status gui
      i_callback_user_command  = 'F_USER_COMMAND'
      i_callback_top_of_page   = 'F_TOP_OF_PAGE' " CONFIGURAÇÃO DO LAYOUT
    TABLES
      t_outtab                 = t_saida[]  " TABELA QUE CONTEM DADOS A SEREM EXIBIDOS NO RELATÓRIO  ALV.
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE s208(00) WITH 'ERRO AO EXIBIR O ALV.' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.


"AÇÕES DO RELATÓRIO ALV.

*ORGANIZAR POR SESSÕES

"5.1  – EFETUAR QUEBRA PELOS CAMPOS EMPRESA, CLIENTE (SE O RADIOBUTTON CLIENTE ESTIVER MARCADO) OU FORNECEDOR (SE O RADIOBUTTON FORNECEDOR ESTIVER MARCADO).
*&---------------------------------------------------------------------
*&      FORM  F_SORT   É RESPONSÁVEL POR DEFINIR AS REGRAS DE CLASSIFICAÇÃO PARA O RELATÓRIO ALV.
"HORA QUE UTILIZAR A ORDENAÇÃO SO SORT NA FIELDCAT, FAZ A QUEBRA PELOS CÓDIGOS
*&---------------------------------------------------------------------
FORM f_sort .
  w_sort-spos      = 1.
  w_sort-fieldname = 'BUKRS'.
  w_sort-tabname   = 'T_SAIDA'.
  w_sort-up        = abap_true.
  w_sort-subtot    = abap_true.
  APPEND w_sort TO t_sort.
  FREE w_sort.

  IF p_kunnr EQ abap_true. "  É RESPONSÁVEL POR DEFINIR AS REGRAS DE CLASSIFICAÇÃO PARA O RELATÓRIO ALV.

    w_sort-fieldname = 'KUNNR'.
    W_SORT-TABNAME   = 'T_SAIDA'.
    w_sort-spos      = 2.
    w_sort-up        = abap_true.
    w_sort-subtot    = abap_true.
    APPEND w_sort TO t_sort. "PARA CLASSIFICAR EM ORDEM ASCENDENTE E W_SORT-SUBTOT É DEFINIDO COMO ABAP_TRUE PARA CALCULAR SUBTOTAIS PARA CADA VALOR ÚNICO DE KUNNR.
    FREE w_sort.              "A ESTRUTURA W_SORT É ENTÃO ANEXADA À TABELA T_SORT E A MEMÓRIA OCUPADA POR W_SORT É LIBERAD

  ELSE.
    
    w_sort-fieldname = 'LIFNR'.
    W_SORT-TABNAME = 'T_SAIDA'.
    w_sort-spos      = 2.
    w_sort-up        = abap_true.
    w_sort-subtot    = abap_true.
    APPEND w_sort TO t_sort.
    FREE w_sort.
    "DEFINIDO COMO ABAP_TRUE PARA CLASSIFICAR EM ORDEM ASCENDENTE E W_SORT-SUBTOT É DEFINIDO COMO ABAP_TRUE PARA CALCULAR SUBTOTAIS PARA CADA VALOR ÚNICO DE KUNNR.
    "A ESTRUTURA W_SORT É ENTÃO ANEXADA À TABELA T_SORT E A MEMÓRIA OCUPADA POR W_SORT É LIBERADA.
    "ESTÁ DEFININDO AS REGRAS DE CLASSIFICAÇÃO PARA O RELATÓRIO ALV COM BASE NAS CONFIGURAÇÕES FORNECIDAS.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------
**&      FORM  USER_COMMAND
*&---------------------------------------------------------------------

*
*SET PARAMETER ID ‘BUK’ FIELD T_SAIDA-BUKRS.    SET PARAMETER ID ‘BLN’ FIELD T_SAIDA-BELNR.
*	 	SET PARAMETER ID ‘GJR’ FIELD T_SAIDA-GJAHR.
*	 	CALL TRANSACTION ‘FB03’ AND SKIP FIRST SCREEN

FORM f_user_command USING p_ucomm     LIKE sy-ucomm
                          p_selfield  TYPE slis_selfield.     " CODIFICAÇºAO PARA IR A ((((((((((((((((TRANSAÇÃO FB03)))))))))))))))))))


  DATA: t_alv_txt    TYPE TABLE OF string,
        t_saida_aux  TYPE TABLE OF ty_saida,
        t_alv_csv    TYPE TABLE OF string,
        w_saida_aux  TYPE ty_saida,
        w_alv_txt    TYPE string,
        w_alv_csv    TYPE string,
        lv_dmbtr     TYPE STRING.
*        lv_path     TYPE string,      " varialvel para alocar o caminha  do GUI_DOWNLOAD
*        lv_resp     TYPE string,      " variavel de responsavel para cliente e fornecedor

**BOTÃO ALV.

  CASE p_ucomm.

    WHEN 'GERAR_TXT'.

      FREE t_saida_aux[].
      t_saida_aux[] = t_saida[].

      DELETE t_saida_aux WHERE mark NE abap_true.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*   IF t_saida_aux IS INITIAL.
*        MESSAGE s208(00) DISPLAY LIKE 'E' WITH text-e01." selecione uma linha
*        RETURN.
*      ENDIF.
*    IF t_saida_aux IS INITIAL.
*      MESSAGE s208(00) DISPLAY LIKE 'E' WITH text-e01." selecione uma linha
*      RETURN.
*    ENDIF.

*  LOOP AT t_saida_aux INTO w_saida_aux.

*    IF w_saida-kunnr IS INITIAL.
*      lv_resp = w_saida_aux-lifnr.          "fornecedor
*    ELSE.
*      lv_resp = w_saida_aux-kunnr.          "cliente
*    ENDIF.

*    FREE lv_dmbtr.
*    lv_dmbtr = w_saida_aux-dmbtr.


*                    w-saida-mark
*                    w_saida-kunnr
*                    w_saida-lifnr
*    CONCATENATE w_saida_aux-bukrs
*                lv_resp
*                w_saida_aux-gjahr
*                w_saida_aux-belnr
*                w_saida_aux-buzei
*                w_saida_aux-blart
*                w_saida_aux-budat
*                w_saida_aux-waers
*                w_saida_aux-monat
*                w_saida_aux-bschl
*                lv_dmbtr         "campo número, ele nao aceita ser concatenado.
*                               "Pra concatenar precisa ser do tipo CHAR, entao criamos uma auxiliar DMBTR char pra receber uma DMBTR numerica e usa-la no comando CONCATENATE.

*         INTO w_alv_csv SEPARATED BY space.
*    APPEND w_alv_csv TO t_alv_csv.

*  ENDLOOP.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CALL FUNCTION 'GUI_DOWNLOAD'
       EXPORTING
          filename                = 'C:\USERS\DUGAB\DESKTOP\TXT.PARTIDAS DE CLIENTES’\PARTIDAS DE CLIENTE.TXT'
        TABLES
          data_tab                = t_saida_aux
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
         OTHERS                  = 22.


    WHEN 'SAVE_CSV'.
*
     LOOP AT t_saida INTO w_saida WHERE mark EQ abap_true.
*
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<TRECHO EDITADO PELO JAPA
*        FREE lv_dmbtr.
*       lv_dmbtr = w_saida-dmbtr.
*

*        CONCATENATE w_saida-mark
*                   w_saida-bukrs
*                    w_saida-kunnr
*                    w_saida-lifnr
*                    w_saida-gjahr
*                    w_saida-belnr
*                    w_saida-buzei
*                    w_saida-blart
*                    w_saida-budat
*                    w_saida-waers
*                    w_saida-monat
*                    w_saida-bschl
*                    lv_dmbtr         "campo número, ele nao aceita ser concatenado.
*                                   "Pra concatenar precisa ser do tipo CHAR, entao criamos uma auxiliar DMBTR char pra receber uma DMBTR numerica e usa-la no comando CONCATENATE.

*             INTO w_alv_csv SEPARATED BY ';'.

*        APPEND w_alv_csv TO t_alv_csv.


*      ENDLOOP.
 IF p_ucomm = 'GERAR_TXT'.

IF p_kunnr IS NOT INITIAL.
  lv_path = c_cliente_txt.
ELSE.
  lv_path = c_fornecedor_txt.
ENDIF.
ENDIF.

*JAPA
CALL FUNCTION 'GUI_DOWNLOAD'
EXPORTING
  filename                = lv_path
TABLES
  data_tab                = t_alv_csv
EXCEPTIONS
  file_write_error        = 1
  no_batch                = 2
  gui_refuse_filetransfer = 3
  invalid_type            = 4
  no_authority            = 5
  unknown_error           = 6
  header_not_allowed      = 7
  separator_not_allowed   = 8
  filesize_not_allowed    = 9
  header_too_long         = 10
  dp_error_create         = 11
  dp_error_send           = 12
  dp_error_write          = 13
  unknown_dp_error        = 14
  access_denied           = 15
  dp_out_of_memory        = 16
  disk_full               = 17
  dp_timeout              = 18
  file_not_found          = 19
  dataprovider_exception  = 20
  control_flush_error     = 21
  OTHERS                  = 22.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = 'C:\USERS\DUGAB\DESKTOP\CSV.PARTIDAS DE CLIENTES\PARTIDAS DE CLIENTE.CSV'
        TABLES
          data_tab                = t_alv_csv
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
    
*JAPA
WHEN 'SAVE_CSV'.

t_saida_aux = t_saida.
DELETE t_saida_aux WHERE mark IS INITIAL.

IF t_saida_aux IS INITIAL.
  MESSAGE s208(00) DISPLAY LIKE 'E' WITH text-e01." SELECIONE UMA LINHA   "messagem   de selecionar o um linha
  RETURN.
ENDIF.

IF w_saida-kunnr IS INITIAL.
    lv_resp = 'FORNECEDOR'.          "fornecedor
  ELSE.
    lv_resp = 'CLIENTE'.          "cliente
  ENDIF.

CONCATENATE 'EMPRESA'                 " cabeçalho ,para o csv EXCELL.
             LV_RESP
             'EXERCÍCIO'
             'DOCUMENTO CONTÁBIL'
             'LINHA DOC. CONTÁBIL'
             'TIPO NO DOCUMENTO'
             'DATA DE LANÇAMENTO'
             'CÓDIGO DA MOEDA'
             'MÊS DO EXERCÍCIO'
             'CHAVE DE LANÇAMENTO'
             'MONTANTE'
             INTO w_alv_csv
             SEPARATED BY ';'.

             APPEND w_alv_csv TO t_alv_csv.




LOOP AT t_saida_aux INTO w_saida_auX.

  IF w_saida_aux-kunnr IS INITIAL.
    lv_resp = w_saida_aux-lifnr.          "fornecedor
  ELSE.
    lv_resp = w_saida_aux-kunnr.          "cliente
  ENDIF.

  FREE lv_dmbtr.
  lv_dmbtr = w_saida_aux-dmbtr.






  *JAPA

                       w-saida-mark
  *                    w_saida-kunnr
  *                    w_saida-lifnr
          CONCATENATE w_saida_aux-bukrs
                      lv_resp
                      w_saida_aux-gjahr
                      w_saida_aux-belnr
                      w_saida_aux-buzei
                      w_saida_aux-blart
                      w_saida_aux-budat
                      w_saida_aux-waers
                      w_saida_aux-monat
                      w_saida_aux-bschl
                      lv_dmbtr         "campo número, ele nao aceita ser concatenado.
                                     "Pra concatenar precisa ser do tipo CHAR, entao criamos uma auxiliar DMBTR char pra receber uma DMBTR numerica e usa-la no comando CONCATENATE.
  
               INTO w_alv_csv SEPARATED BY ';'.
  
          APPEND w_alv_csv TO t_alv_csv.
        ENDLOOP.
  
        IF p_ucomm = 'SAVE_CSV'.
  
          IF p_kunnr IS NOT INITIAL.
            lv_path = c_cliente_csv.
          ELSE.
            lv_path = c_fornecedor_csv.
          ENDIF.
        ENDIF.
  
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = lv_path
          TABLES
            data_tab                = t_alv_csv
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.

    WHEN 'VOLTAR'.
      SET SCREEN 0.

    WHEN 'SAIR'.
      SET SCREEN 0.

    WHEN 'FECHAR'.
      SET SCREEN 0.


    WHEN OTHERS.

      CASE p_ucomm.  " P_UCOMM É O COMANDO DO USUÁRIO

        WHEN OTHERS.
          " P_SELFIELD É A LINHA SELECIONADA NO RELATÓRIO ALV
          IF p_selfield-fieldname = 'BELNR'.  "ESTA LINHA VERIFICA SE O CAMPO SELECIONADO NO RELATÓRIO ALV É BELNR.
            READ TABLE t_saida INTO w_saida INDEX p_selfield-tabindex.

            IF sy-subrc EQ 0.

              "ESTAS LINHAS ESTÃO DEFININDO ALGUNS PARÂMETROS E, EM SEGUIDA, CHAMANDO A TRANSAÇÃO FB03 COM ESSES PARÂMETROS. OS PARÂMETROS SÃO DEFINIDOS COM BASE NOS VALORES DOS CAMPOS BUKRS, BELNR E GJAHR NA LINHA SELECIONADA DO RELATÓRIO ALV.
              SET PARAMETER ID 'BUK' FIELD w_saida-bukrs.
              SET PARAMETER ID 'BLN' FIELD w_saida-belnr.
              SET PARAMETER ID 'GJR' FIELD w_saida-gjahr.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.       " ((((((((((((FB03)))))))))))

            ENDIF.
          ENDIF.

      ENDCASE.

      "ENTÃO, BASICAMENTE, ESSA ROTINA F_USER_COMMAND ESTÁ LIDANDO COM OS COMANDOS DO USUÁRIO NO RELATÓRIO ALV E, SE O CAMPO BELNR FOR SELECIONADO, ELA CHAMA A TRANSAÇÃO FB03 COM OS PARÂMETROS APROPRIADOS.

  ENDCASE.
ENDFORM.
*********************MONAT ****************************************************************************************************************************************************
*********************BSCHL ****************************************************************************************************************************************************
*FORMULÁRIO Z_STATUSDMBTR   É UMA MANEIRA DE ORGANIZAR O CODIGO EM BLOCOS REUTILIZAVEIS
*
*PARAMÊTROS          "PARAMÊTROS
FORM zf_status USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS 'Z_PROVA_STATUS_GUI'. "FUNÇÃO SET PF-STATUS.  "PARA DEFINIR O STATUS DA INTERFACE DO USUÁRIO COMO 'Z_GUI_AL'.
ENDFORM.
" ISSO PODE SER ÚTIL, POR EXEMPLO, SE VOCÊ QUISER ALTERAR O STATUS DA INTERFACE DO USUÁRIO EM VÁRIAS PARTES DO CÓDIGO.
" EM VEZ DE REPETIR O CÓDIGO PARA DEFINIR O STATUS EM CADA LUGAR, VOCÊ PODE SIMPLESMENTE CHAMAR O FORMULÁRIO Z_STATUS.
