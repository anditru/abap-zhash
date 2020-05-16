*&---------------------------------------------------------------------*
*& Report Z_HASH_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_HASH_TEST.

PARAMETERS:
      p_level  TYPE decfloat16,
      p_type   TYPE int4,
      p_length TYPE int4.

DATA:
      lo_hashtable        TYPE REF TO zcl_hashtable,
      lv_level            TYPE f,
      lv_colisions_succ   TYPE int4,
      lv_succ_avg         TYPE decfloat34,
      lv_colisions_nosucc TYPE int4,
      lv_nosucc_avg       TYPE decfloat34,
      lv_time_avg         TYPE decfloat34.


CREATE OBJECT lo_hashtable
  EXPORTING
    iv_length = p_length
    iv_type = p_type.

lv_level = CONV f( p_level ).
IF p_type = 0.
  CALL METHOD lo_hashtable->mod_hash_linear
    EXPORTING
      iv_level = lv_level.
ELSEIF p_type = 1.
  CALL METHOD lo_hashtable->mod_hash_quadratic
    EXPORTING
      iv_level = lv_level.
ENDIF.
CALL METHOD lo_hashtable->measure_colisions
  IMPORTING
    ev_colisions_succ = lv_colisions_succ
    ev_colisions_nosucc = lv_colisions_nosucc
    ev_time_avg = lv_time_avg.

lv_succ_avg = ROUND( val = ( lv_colisions_succ / lo_hashtable->mv_length ) dec = 2 ).
lv_nosucc_avg = ROUND( val = ( lv_colisions_nosucc / lo_hashtable->mv_length ) dec = 2 ).

WRITE: 'Length:', (15) lo_hashtable->mv_length, '    Level:', (3) p_level, '   Type (0 = linear probing, 1 = quadratic probing):', (3) p_type, /,
       (10) '',        (20) 'successful colisions', (24) '  unsuccessful colisions', /,
       (10) 'absolute',(21) lv_colisions_succ,      (24) lv_colisions_nosucc,      /,
       (10) 'per key', (20) lv_succ_avg,            (24) lv_nosucc_avg,            /,
       (10) lv_time_avg, 'ms average access time'.
