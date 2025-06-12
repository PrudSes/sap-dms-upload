REPORT ztest_dms_upload.

PARAMETERS:
  p_file    TYPE rlgrap-filename OBLIGATORY,
  p_path    TYPE string OBLIGATORY,
  p_repoid  TYPE string OBLIGATORY.

CONSTANTS:
  c_client_id     TYPE string VALUE 'test12234',
  c_client_secret TYPE string VALUE 'test12234',
  c_token_url     TYPE string VALUE 'https://xx-btp-dev.authentication.us10.hana.ondemand.com/oauth/token',
  c_dms_url       TYPE string VALUE 'https://xx-btp-dev.authentication.us10.hana.ondemand.com/v1/documents',
  c_boundary      TYPE string VALUE '----WebKitFormBoundary7MA4YWxkTrZu0gW',
  c_crlf          TYPE string VALUE cl_abap_char_utilities=>cr_lf.

TYPES: BEGIN OF ty_response,
         access_token TYPE string,
         token_type   TYPE string,
         expires_in   TYPE string,
         scope        TYPE string,
         jti          TYPE string,
       END OF ty_response.

DATA:
  lv_file_length      TYPE i,
  lv_xstring          TYPE xstring,
  lt_binary_data      TYPE STANDARD TABLE OF x255,
  lv_token            TYPE string,
  lv_filename         TYPE string,
  lv_payload          TYPE xstring,
  lv_multipart        TYPE string,
  lv_multipart_x      TYPE xstring,
  lv_end_boundary     TYPE string,
  lv_end_boundary_x   TYPE xstring,
  lv_response         TYPE string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.

CLASS lcl_oauth DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_token
      IMPORTING
        iv_client_id     TYPE string
        iv_client_secret TYPE string
        iv_token_url     TYPE string
      RETURNING VALUE(rv_token) TYPE string.
ENDCLASS.

CLASS lcl_oauth IMPLEMENTATION.
  METHOD get_token.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_body        TYPE string,
          lv_response    TYPE string,
          ls_token       TYPE ty_response.

    cl_http_client=>create_by_url(
      EXPORTING url = iv_token_url
      IMPORTING client = lo_http_client ).

    lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/x-www-form-urlencoded' ).

    lv_body = |grant_type=client_credentials&client_id={ iv_client_id }&client_secret={ iv_client_secret }|.
    lo_http_client->request->set_cdata( lv_body ).

    lo_http_client->send( ).
    lo_http_client->receive( ).

    lv_response = lo_http_client->response->get_cdata( ).

    /ui2/cl_json=>deserialize(
      EXPORTING json = lv_response
      CHANGING  data = ls_token ).

    rv_token = ls_token-access_token.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename   = p_file
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_file_length
    CHANGING
      data_tab   = lt_binary_data
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    WRITE: 'File upload failed.'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_file_length
    IMPORTING
      buffer       = lv_xstring
    TABLES
      binary_tab   = lt_binary_data
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc <> 0.
    WRITE: 'Conversion to XSTRING failed.'.
    EXIT.
  ENDIF.

  lv_filename = p_file.

  lv_token = lcl_oauth=>get_token(
    iv_client_id     = c_client_id
    iv_client_secret = c_client_secret
    iv_token_url     = c_token_url ).

  CONCATENATE '--' c_boundary c_crlf
              'Content-Disposition: form-data; name="metadata"' c_crlf
              'Content-Type: application/json' c_crlf c_crlf
              '{"parentPath":"' p_path '"}' c_crlf
              '--' c_boundary c_crlf
              'Content-Disposition: form-data; name="file"; filename="' lv_filename '"' c_crlf
              'Content-Type: application/octet-stream' c_crlf c_crlf
         INTO lv_multipart.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_multipart
    IMPORTING
      buffer = lv_multipart_x.

  lv_payload = lv_multipart_x && lv_xstring.

  CONCATENATE c_crlf '--' c_boundary '--' c_crlf INTO lv_end_boundary.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_end_boundary
    IMPORTING
      buffer = lv_end_boundary_x.

  lv_payload = lv_payload && lv_end_boundary_x.

  DATA: lo_http_client TYPE REF TO if_http_client.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url    = c_dms_url
    IMPORTING
      client = lo_http_client
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc <> 0.
    WRITE: 'HTTP Client creation failed.'.
    EXIT.
  ENDIF.

  lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
  lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_token }| ).
  lo_http_client->request->set_header_field( name = 'Content-Type' value = |multipart/form-data; boundary={ c_boundary }| ).
  lo_http_client->request->set_data( lv_payload ).

  lo_http_client->send( ).
  lo_http_client->receive( ).

  lv_response = lo_http_client->response->get_cdata( ).
  WRITE: / 'DMS Upload Response:', lv_response.
