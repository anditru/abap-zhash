class ZCL_HASHTABLE definition
  public
  final
  create public .

public section.

  data MT_TABLE type ZTHASHTABLE .
  data MV_LENGTH type INT4 .
  data MV_COLISIONS type INT4 .
  data MV_TYPE type INT4 .

  methods CONSTRUCTOR
    importing
      !IV_LENGTH type INT4
      !IV_TYPE type INT4 .
  methods MOD_HASH_LINEAR
    importing
      !IV_KEY type INT4
    exporting
      !EV_HASH type INT4 .
  methods MOD_HASH_QUADRATIC
    importing
      !IV_KEY type INT4
    exporting
      !EV_HASH type INT4 .
  methods MEASURE_COLISIONS
    exporting
      !EV_COLISIONS_SUCC type INT4
      !EV_SUCC_AVG type DECFLOAT16
      !EV_COLISIONS_NOSUCC type INT4
      !EV_NOSUCC_AVG type DECFLOAT16
      !EV_COLISIONS_TOTAL type INT4
      !EV_TOTAL_AVG type DECFLOAT16
      !EV_TIME_AVG type DECFLOAT16 .
  methods FILL_TABLE
    importing
      !IV_LEVEL type DECFLOAT16 .
  methods PRIME_TEST
    importing
      !IV_NUM type INT4
    exporting
      !EV_RES type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HASHTABLE IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA:
          ls_line   TYPE zshashtable,
          lv_count  TYPE f VALUE 0,
          lv_length TYPE int4,
          lv_prime  TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

    ASSIGN ls_line TO <fs_line>.

    me->mv_type = iv_type.

    IF iv_type = 0.
      me->mv_length = iv_length.
    ELSEIF iv_type = 1.
      WHILE me->mv_length LT iv_length OR lv_prime = abap_false.
        me->mv_length = 4 * lv_count + 3.
        CALL METHOD me->prime_test
          EXPORTING
            iv_num = me->mv_length
          IMPORTING
            ev_res = lv_prime.
        lv_count = lv_count + 1.
      ENDWHILE.
    ENDIF.

    lv_count = 0.
    DO me->mv_length TIMES.
      <fs_line>-hash = lv_count.
      INSERT <fs_line> INTO TABLE me->mt_table.
      lv_count = lv_count + 1.
    ENDDO.

  endmethod.


  method FILL_TABLE.
    DATA:
      lv_key   TYPE int4,
      lv_hash  TYPE int4,
      lv_count TYPE int4 VALUE 1.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

    DO ( me->mv_length * iv_level ) TIMES.
*   Get random integer
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max = 1000000
          ran_int_min = 0
        IMPORTING
          ran_int = lv_key.
*     Generate hash
      IF me->mv_type = 0.
        CALL METHOD me->mod_hash_linear
          EXPORTING
            iv_key = lv_key
          IMPORTING
            ev_hash = lv_hash.
      ELSEIF me->mv_type = 1.
        CALL METHOD me->mod_hash_quadratic
          EXPORTING
            iv_key = lv_key
          IMPORTING
            ev_hash = lv_hash.
      ENDIF.
*     Insert key
      READ TABLE me->mt_table INDEX ( lv_hash + 1 ) ASSIGNING <fs_line>.
      <fs_line>-key = lv_key.
      lv_count = 1.
    ENDDO.
  endmethod.


  method MEASURE_COLISIONS.

    DATA:
          lv_hash_current     TYPE int4,
          lv_hash_initial     TYPE int4,
          lv_count            TYPE int4 VALUE 1,
          lv_key              TYPE int4,
          lv_start_time       TYPE int4,
          lv_end_time         TYPE int4,
          lv_time_sum         TYPE int4 VALUE 0,
          lv_nr_keys          TYPE int4.

    FIELD-SYMBOLS:
                   <fs_line>     TYPE zshashtable,
                   <fs_line_out> TYPE zshashtable.


    LOOP AT me->mt_table ASSIGNING <fs_line_out>.
      GET RUN TIME FIELD lv_start_time.
      lv_key = <fs_line_out>-key.
*     Generate hash
      lv_hash_initial = lv_key MOD me->mv_length.
      lv_hash_current = lv_hash_initial.
      READ TABLE me->mt_table INDEX ( lv_hash_initial + 1 ) ASSIGNING <fs_line>.
*     Check if hash holds correct element, if not start probing
      IF me->mv_type = 0. "For linear probing
        WHILE <fs_line>-key NE lv_key.
          lv_hash_current = ( lv_hash_initial + lv_count ) MOD me->mv_length.
          READ TABLE me->mt_table INDEX ( lv_hash_current + 1 ) ASSIGNING <fs_line>.
          lv_count = lv_count + 1.
        ENDWHILE.
      ELSEIF me->mv_type = 1. "For quadratic probing
        WHILE <fs_line>-key NE lv_key.
          lv_hash_current = ( lv_hash_initial + ( ( -1 ) ** ( lv_count + 1 ) * ( CEIL(  lv_count / 2 ) ** 2 ) ) ) MOD me->mv_length.
          READ TABLE me->mt_table INDEX ( lv_hash_current + 1 ) ASSIGNING <fs_line>.
          lv_count = lv_count + 1.
        ENDWHILE.
      ENDIF.

      GET RUN TIME FIELD lv_end_time.
      lv_time_sum = lv_time_sum + ( lv_end_time - lv_start_time ).

      IF lv_count = 2.
        ev_colisions_succ = ev_colisions_succ + 1.
      ELSEIF lv_count GT 2.
        ev_colisions_succ = ev_colisions_succ + 1.
        ev_colisions_nosucc = ev_colisions_nosucc + ( lv_count - 2 ).
      ENDIF.
      lv_count = 1.

    ENDLOOP.
    lv_nr_keys = LINES( me->mt_table ).

    ev_succ_avg = ROUND( val = ( ev_colisions_succ / me->mv_length ) dec = 2 ).
    ev_nosucc_avg = ROUND( val = ( ev_colisions_nosucc / me->mv_length ) dec = 2 ).
    ev_colisions_total = ev_colisions_succ + ev_colisions_nosucc.
    ev_total_avg = ROUND( val = ( ev_colisions_total / me->mv_length ) dec = 2 ).

    ev_time_avg = CONV decfloat34( ( lv_time_sum / lv_nr_keys ) ).

  endmethod.


  method MOD_HASH_LINEAR.

    DATA:
          ls_line         TYPE zshashtable,
          lv_hash_initial TYPE int4,
          lv_count        TYPE int4 VALUE 1.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

*   Generate initial hash
    lv_hash_initial = iv_key MOD me->mv_length.
    ev_hash = lv_hash_initial.
    READ TABLE me->mt_table INDEX ( ev_hash + 1 ) ASSIGNING <fs_line>.
*   Check if hash is taken, if so start probing
    WHILE <fs_line>-key IS NOT INITIAL AND <fs_line>-key NE iv_key.
      ev_hash = ( lv_hash_initial + lv_count ) MOD me->mv_length.
      READ TABLE me->mt_table INDEX ( ev_hash + 1 ) ASSIGNING <fs_line>.
      lv_count = lv_count + 1.
    ENDWHILE.

  endmethod.


  method MOD_HASH_QUADRATIC.

    DATA:
          ls_line         TYPE zshashtable,
          lv_hash_initial TYPE int4,
          lv_count        TYPE int4 VALUE 1.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

*   Generate initial hash
    lv_hash_initial = iv_key MOD me->mv_length.
    ev_hash = lv_hash_initial.
    READ TABLE me->mt_table INDEX ( ev_hash + 1 ) ASSIGNING <fs_line>.
*   Check if hash is taken, if so start probing
    WHILE <fs_line>-key IS NOT INITIAL AND <fs_line>-key NE iv_key.
      ev_hash = ( lv_hash_initial + ( ( -1 ) ** ( lv_count + 1 ) * ( CEIL(  lv_count / 2 ) ** 2 ) ) ) MOD me->mv_length.
      READ TABLE me->mt_table INDEX ( ev_hash + 1 ) ASSIGNING <fs_line>.
      lv_count = lv_count + 1.
    ENDWHILE.

  endmethod.


  method PRIME_TEST.

    DATA:
          lv_limit TYPE f,
          lv_count TYPE int4 VALUE 2,
          lv_result TYPE int4.

    ev_res = abap_true.
    WHILE lv_count LE SQRT( iv_num ).
      lv_result = iv_num MOD lv_count.
      IF lv_result = 0.
        ev_res = abap_false.
        EXIT.
      ENDIF.
      lv_count = lv_count + 1.
    ENDWHILE.

  endmethod.
ENDCLASS.
