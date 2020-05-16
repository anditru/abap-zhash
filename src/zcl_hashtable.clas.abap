class ZCL_HASHTABLE definition
  public
  final
  create public .

public section.

  data MT_TABLE type ZTHASHTABLE .
  data MV_LENGTH type F .
  data MV_COLISIONS type INT4 .
  data MV_TYPE type INT4 .

  methods CONSTRUCTOR
    importing
      !IV_LENGTH type INT4
      !IV_TYPE type INT4 .
  methods MOD_HASH_LINEAR
    importing
      !IV_LEVEL type F .
  methods MOD_HASH_QUADRATIC
    importing
      !IV_LEVEL type F .
  methods MEASURE_COLISIONS
    exporting
      !EV_COLISIONS_SUCC type INT4
      !EV_COLISIONS_NOSUCC type INT4
      !EV_TIME_AVG type DECFLOAT34 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HASHTABLE IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA:
          ls_line  TYPE zshashtable,
          lv_count TYPE f VALUE 0.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

    ASSIGN ls_line TO <fs_line>.

    me->mv_length = iv_length.
    me->mv_type = iv_type.

    DO iv_length TIMES.
      <fs_line>-hash = lv_count.
      INSERT <fs_line> INTO TABLE me->mt_table.
      lv_count = lv_count + 1.
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
          lv_hash_current = ( lv_hash_initial + ( lv_count * lv_count ) ) MOD me->mv_length.
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
    ev_time_avg = CONV decfloat34( ( lv_time_sum / lv_nr_keys ) / 1000 ).

  endmethod.


  method MOD_HASH_LINEAR.

    DATA:
          ls_line         TYPE zshashtable,
          lv_key          TYPE int4,
          lv_hash_initial TYPE decfloat34,
          lv_hash_current TYPE decfloat34,
          lv_count        TYPE int4 VALUE 1.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

    DO ( me->mv_length * iv_level ) TIMES.
*   Get random integer
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max = 1000000000
          ran_int_min = 0
        IMPORTING
          ran_int = lv_key.
*     Generate hash
      lv_hash_initial = lv_key MOD me->mv_length.
      lv_hash_current = lv_hash_initial.
      READ TABLE me->mt_table WITH TABLE KEY hash = lv_hash_initial ASSIGNING <fs_line>.
*     Check if hash is taken, if so start probing
      WHILE <fs_line>-key IS NOT INITIAL.
        lv_hash_current = ( lv_hash_initial + lv_count ) MOD me->mv_length.
        READ TABLE me->mt_table WITH TABLE KEY hash = lv_hash_current ASSIGNING <fs_line>.
        lv_count = lv_count + 1.
      ENDWHILE.
*     Insert key
      <fs_line>-key = lv_key.
      lv_count = 1.
    ENDDO.

  endmethod.


  method MOD_HASH_QUADRATIC.

    DATA:
          ls_line         TYPE zshashtable,
          lv_key          TYPE int4,
          lv_hash_initial TYPE decfloat34,
          lv_hash_current TYPE decfloat34,
          lv_count        TYPE int4 VALUE 1.

    FIELD-SYMBOLS:
                   <fs_line> TYPE zshashtable.

    DO ( me->mv_length * iv_level ) TIMES.
*   Get random integer
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max = 1000000000
          ran_int_min = 0
        IMPORTING
          ran_int = lv_key.
*     Generate hash
      lv_hash_initial = lv_key MOD me->mv_length.
      lv_hash_current = lv_hash_initial.
      READ TABLE me->mt_table WITH TABLE KEY hash = lv_hash_initial ASSIGNING <fs_line>.
*     Check if hash is taken, if so start probing
      WHILE <fs_line>-key IS NOT INITIAL.
        lv_hash_current = ( lv_hash_initial + ( lv_count * lv_count ) ) MOD me->mv_length.
        READ TABLE me->mt_table WITH TABLE KEY hash = lv_hash_current ASSIGNING <fs_line>.
        lv_count = lv_count + 1.
      ENDWHILE.
*     Insert key
      <fs_line>-key = lv_key.
      lv_count = 1.
    ENDDO.

  endmethod.
ENDCLASS.
