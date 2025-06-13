REPORT zupload_dms_multipart.

DATA: lv_url         TYPE string VALUE 'https://api-sdm-di.cfapps.us10.hana.ondemand.com/browser/ZDMSCG/root/June2025',
      lv_boundary    TYPE string VALUE '----WebKitFormBoundary7MA4YWxkTrZu0gW',
      lv_crlf        TYPE string VALUE cl_abap_char_utilities=>cr_lf,
      lv_body        TYPE string,
      lv_file_path   TYPE string VALUE 'C:\temp\DMS.CMIS.postman_collection.json',
      lv_filename    TYPE string VALUE 'DMS.CMIS.postman_collection.json',
      lt_file_data   TYPE STANDARD TABLE OF x255,
      lv_file_string TYPE xstring,
      lv_file_b64    TYPE string.

DATA: lo_http_client TYPE REF TO if_http_client,
      lv_request     TYPE string,
      lv_response    TYPE string.

" Step 1: Upload file from frontend
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename                = lv_file_path
    filetype                = 'BIN'
  IMPORTING
    filelength              = DATA(lv_file_length)
  TABLES
    data_tab                = lt_file_data
  EXCEPTIONS
    file_open_error         = 1
    file_read_error         = 2
    others                  = 3.

" Convert to xstring
CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
  EXPORTING
    input_length = lv_file_length
  IMPORTING
    buffer       = lv_file_string
  TABLES
    binary_tab   = lt_file_data.

" Build multipart/form-data
CONCATENATE
  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="propertyId[0]"' && lv_crlf && lv_crlf &&
  'cmis:name' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="propertyValue[0]"' && lv_crlf && lv_crlf &&
  'pdx.txt' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="propertyId[1]"' && lv_crlf && lv_crlf &&
  'cmis:objectTypeId' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="propertyValue[1]"' && lv_crlf && lv_crlf &&
  'cmis:document' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="succinct"' && lv_crlf && lv_crlf &&
  'true' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="filename"' && lv_crlf && lv_crlf &&
  'pdx.txt' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="includeAllowableActions"' && lv_crlf && lv_crlf &&
  'true' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="cmisaction"' && lv_crlf && lv_crlf &&
  'createDocument' && lv_crlf &&

  '--' && lv_boundary && lv_crlf &&
  'Content-Disposition: form-data; name="media"; filename="' && lv_filename && '"' && lv_crlf &&
  'Content-Type: application/json' && lv_crlf && lv_crlf
INTO lv_body.

" Append binary file content to body
DATA(lv_payload_x) = cl_abap_conv_in_ce=>create( )->convert( text = lv_body ).
CONCATENATE lv_payload_x lv_file_string INTO lv_payload_x IN BYTE MODE.

" Add closing boundary
DATA(lv_end_boundary) = lv_crlf && '--' && lv_boundary && '--' && lv_crlf.
DATA(lv_end_boundary_x) = cl_abap_conv_in_ce=>create( )->convert( text = lv_end_boundary ).
CONCATENATE lv_payload_x lv_end_boundary_x INTO lv_payload_x IN BYTE MODE.

" Create HTTP client
CALL METHOD cl_http_client=>create_by_url
  EXPORTING
    url                = lv_url
  IMPORTING
    client             = lo_http_client
  EXCEPTIONS
    argument_not_found = 1
    plugin_not_active  = 2
    internal_error     = 3
    others             = 4.

" Set headers
lo_http_client->request->set_header_field( name = 'Content-Type' value = |multipart/form-data; boundary={ lv_boundary }| ).
lo_http_client->request->set_method( 'POST' ).
lo_http_client->request->set_cdata( lv_payload_x ).

" Send request
CALL METHOD lo_http_client->send
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3.

CALL METHOD lo_http_client->receive
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3.

" Get response
DATA(lv_result) = lo_http_client->response->get_cdata( ).
WRITE: / 'Response:', lv_result.
