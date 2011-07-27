# Generated dependency rules
# 
# ethread lib objects...
$(r_OBJ_DIR)/ethr_aux.o: common/ethr_aux.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h ../include/internal/ethr_internal.h \
 ../include/internal/erl_misc_utils.h
$(r_OBJ_DIR)/ethr_atomics.o: common/ethr_atomics.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h ../include/internal/ethr_internal.h \
 ../include/internal/erl_misc_utils.h
$(r_OBJ_DIR)/ethr_mutex.o: common/ethr_mutex.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h ../include/internal/ethr_internal.h \
 ../include/internal/erl_misc_utils.h
$(r_OBJ_DIR)/ethr_cbf.o: common/ethr_cbf.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h
$(r_OBJ_DIR)/ethread.o: pthread/ethread.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h ../include/internal/ethr_internal.h \
 ../include/internal/erl_misc_utils.h
$(r_OBJ_DIR)/ethr_event.o: pthread/ethr_event.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/ethread.h ../include/internal/erl_errno.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h
# erts_internal_r lib objects...
$(r_OBJ_DIR)/erl_printf_format.o: common/erl_printf_format.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_errno.h ../include/internal/erl_printf.h \
 ../include/internal/erl_printf_format.h
$(r_OBJ_DIR)/erl_printf.o: common/erl_printf.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_errno.h ../include/internal/erl_printf.h \
 ../include/internal/erl_printf_format.h
$(r_OBJ_DIR)/erl_misc_utils.o: common/erl_misc_utils.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h
# erts_internal lib objects...
$(OBJ_DIR)/erl_printf_format.o: common/erl_printf_format.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_errno.h ../include/internal/erl_printf.h \
 ../include/internal/erl_printf_format.h
$(OBJ_DIR)/erl_printf.o: common/erl_printf.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_errno.h ../include/internal/erl_printf.h \
 ../include/internal/erl_printf_format.h
$(OBJ_DIR)/erl_misc_utils.o: common/erl_misc_utils.c \
 /home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/$(TARGET)/config.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h
# erts_r lib objects...
$(r_OBJ_DIR)/erl_memory_trace_parser.o: common/erl_memory_trace_parser.c \
 ../include/erl_memory_trace_parser.h \
 ../include/erl_fixed_size_int_types.h \
 ../include/$(TARGET)/erl_int_sizes_config.h \
 ../include/internal/erl_memory_trace_protocol.h
# erts lib objects...
$(OBJ_DIR)/erl_memory_trace_parser.o: common/erl_memory_trace_parser.c \
 ../include/erl_memory_trace_parser.h \
 ../include/erl_fixed_size_int_types.h \
 ../include/$(TARGET)/erl_int_sizes_config.h \
 ../include/internal/erl_memory_trace_protocol.h
# EOF
