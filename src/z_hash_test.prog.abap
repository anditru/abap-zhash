*&---------------------------------------------------------------------*
*& Report Z_HASH_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_HASH_TEST.

PARAMETERS:
      p_nr     TYPE int4,
      p_level  TYPE decfloat16,
      p_type   TYPE int4,
      p_length TYPE int4.

DATA:
      lo_hashtable          TYPE REF TO zcl_hashtable,
      lv_colisions_succ     TYPE decfloat16,
      lv_succ_avg           TYPE decfloat16,
      lv_colisions_nosucc   TYPE decfloat16,
      lv_nosucc_avg         TYPE decfloat16,
      lv_colisions_total    TYPE decfloat16,
      lv_total_avg          TYPE decfloat16,
      lv_time_avg           TYPE decfloat16,
      lv_colisions_succ_i   TYPE int4,
      lv_succ_avg_i         TYPE decfloat16,
      lv_colisions_nosucc_i TYPE int4,
      lv_nosucc_avg_i       TYPE decfloat16,
      lv_colisions_total_i  TYPE int4,
      lv_total_avg_i        TYPE decfloat16,
      lv_time_avg_i         TYPE decfloat16.

DO p_nr TIMES.
  CLEAR lo_hashtable.
  CREATE OBJECT lo_hashtable
    EXPORTING
      iv_length = p_length
      iv_type   = p_type.

  CALL METHOD lo_hashtable->fill_table
    EXPORTING
      iv_level = p_level.

  CALL METHOD lo_hashtable->measure_colisions
    IMPORTING
      ev_colisions_succ   = lv_colisions_succ_i
      ev_succ_avg         = lv_succ_avg_i
      ev_colisions_nosucc = lv_colisions_nosucc_i
      ev_nosucc_avg       = lv_nosucc_avg_i
      ev_colisions_total  = lv_colisions_total_i
      ev_total_avg        = lv_total_avg_i
      ev_time_avg         = lv_time_avg_i.

  lv_colisions_succ = lv_colisions_succ + lv_colisions_succ_i.
  lv_succ_avg = lv_succ_avg + lv_succ_avg_i.
  lv_colisions_nosucc = lv_colisions_nosucc + lv_colisions_nosucc_i.
  lv_nosucc_avg = lv_nosucc_avg + lv_nosucc_avg_i.
  lv_colisions_total = lv_colisions_total + lv_colisions_total_i.
  lv_total_avg = lv_total_avg + lv_total_avg_i.
  lv_time_avg = lv_time_avg + lv_time_avg_i.
  CLEAR:
    lv_colisions_succ_i,
    lv_succ_avg_i,
    lv_colisions_nosucc_i,
    lv_nosucc_avg_i,
    lv_colisions_total_i,
    lv_total_avg_i,
    lv_time_avg_i.
ENDDO.

lv_colisions_succ = ROUND( val = ( lv_colisions_succ / p_nr ) dec = 0 ).
lv_succ_avg = ROUND( val = ( lv_succ_avg / p_nr ) dec = 2 ).
lv_colisions_nosucc = ROUND( val = ( lv_colisions_nosucc / p_nr ) dec = 0 ).
lv_nosucc_avg = ROUND( val = ( lv_nosucc_avg / p_nr ) dec = 2 ).
lv_colisions_total = ROUND( val = ( lv_colisions_total / p_nr ) dec = 0 ).
lv_total_avg = ROUND( val = ( lv_total_avg / p_nr ) dec = 2 ).
lv_time_avg = ROUND( val = ( lv_time_avg / p_nr ) dec = 6 ).

WRITE: /,
       'Number of tables:', (6) p_nr, (11) '    Length:', (6) lo_hashtable->mv_length, (10) '    Level:', (4) p_level, '   Type (0 - linear probing, 1 - quadratic probing):', (3) p_type, /.
ULINE.
WRITE: /,
       'AVERGAE COLISIONS:', /, /,
       (10) '',        (20) 'successful colisions', (24) '  unsuccessful colisions', (15) '          total',  /,
       (10) 'absolute',(20) lv_colisions_succ,      (24) lv_colisions_nosucc,        (15) lv_colisions_total, /,
       (10) 'per key', (20) lv_succ_avg,            (24) lv_nosucc_avg,              (15) lv_total_avg,       /,
       /,
       (10) lv_time_avg, 'microseconds average access time'.
