$(OBJDIR)/atom.o: beam/atom.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_node_container_utils.h beam/hash.h beam/index.h beam/atom.h \
 i686-pc-linux-gnu/erl_atom_table.h beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/beam_bif_load.o: beam/beam_bif_load.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/beam_bp.h \
 beam/beam_catches.h beam/erl_binary.h beam/erl_nif.h \
 beam/erl_nif_api_funcs.h
$(OBJDIR)/beam_bp.o: beam/beam_bp.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/error.h beam/erl_binary.h \
 beam/beam_bp.h
$(OBJDIR)/beam_catches.o: beam/beam_catches.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/beam_catches.h
$(OBJDIR)/beam_debug.o: beam/beam_debug.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/beam_bp.h \
 beam/erl_binary.h
$(OBJDIR)/beam_emu.o: beam/beam_emu.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_nmgc.h \
 beam/error.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/big.h \
 beam/erl_binary.h beam/erl_bits.h beam/dist.h beam/beam_bp.h \
 beam/beam_catches.h hipe/hipe_mode_switch.h beam/error.h \
 hipe/hipe_bif1.h $(TTF_DIR)/beam_hot.h \
 $(TTF_DIR)/beam_cold.h
$(OBJDIR)/beam_load.o: beam/beam_load.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h \
 i686-pc-linux-gnu/erl_version.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_bits.h \
 beam/beam_catches.h beam/erl_binary.h beam/erl_zlib.h zlib/zlib.h \
 zlib/zconf.h hipe/hipe_bif0.h hipe/hipe_mode_switch.h beam/error.h \
 hipe/hipe_arch.h hipe/hipe_x86.h \
 $(TTF_DIR)/beam_pred_funcs.h \
 $(TTF_DIR)/beam_tr_funcs.h
$(OBJDIR)/benchmark.o: beam/benchmark.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/bif.o: beam/bif.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/erl_sys_driver.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_node_container_utils.h beam/hash.h beam/index.h beam/atom.h \
 i686-pc-linux-gnu/erl_atom_table.h beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_binary.h beam/beam_bp.h \
 beam/erl_db_util.h
$(OBJDIR)/big.o: beam/big.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/big.h beam/error.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h
$(OBJDIR)/binary.o: beam/binary.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_binary.h \
 beam/erl_bits.h
$(OBJDIR)/break.o: beam/break.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/version.h \
 beam/error.h beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_instrument.h beam/erl_mtrace.h
$(OBJDIR)/copy.o: beam/copy.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_gc.h \
 beam/erl_nmgc.h beam/big.h beam/erl_binary.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_bits.h
$(OBJDIR)/dist.o: beam/dist.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/dist.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_binary.h
$(OBJDIR)/elib_memmove.o: beam/elib_memmove.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h
$(OBJDIR)/erl_afit_alloc.o: beam/erl_afit_alloc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_afit_alloc.h \
 sys/common/erl_mseg.h beam/sys.h
$(OBJDIR)/erl_alloc.o: beam/erl_alloc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_db.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_db_hash.h beam/erl_db_tree.h beam/erl_binary.h beam/erl_bits.h \
 beam/erl_instrument.h beam/erl_mtrace.h sys/common/erl_mseg.h beam/sys.h \
 sys/common/erl_check_io.h beam/erl_sys_driver.h sys/common/erl_poll.h \
 beam/erl_port_task.h beam/erl_goodfit_alloc.h beam/erl_bestfit_alloc.h \
 beam/erl_afit_alloc.h
$(OBJDIR)/erl_alloc_util.o: beam/erl_alloc_util.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/big.h \
 beam/erl_mtrace.h sys/common/erl_mseg.h beam/sys.h
$(OBJDIR)/erl_arith.o: beam/erl_arith.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h
$(OBJDIR)/erl_async.o: beam/erl_async.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_process_dict.h beam/erl_bif_timer.h \
 beam/erl_time.h hipe/hipe_process.h beam/erl_alloc.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_bestfit_alloc.o: beam/erl_bestfit_alloc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h \
 beam/erl_bestfit_alloc.h sys/common/erl_mseg.h beam/sys.h
$(OBJDIR)/erl_bif_binary.o: beam/erl_bif_binary.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_binary.h \
 beam/erl_bits.h
$(OBJDIR)/erl_bif_chksum.o: beam/erl_bif_chksum.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_binary.h beam/big.h \
 zlib/zlib.h zlib/zconf.h
$(OBJDIR)/erl_bif_ddll.o: beam/erl_bif_ddll.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h
$(OBJDIR)/erl_bif_guard.o: beam/erl_bif_guard.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_binary.h
$(OBJDIR)/erl_bif_info.o: beam/erl_bif_info.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_nmgc.h \
 beam/error.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/big.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_db_util.h beam/erl_binary.h \
 beam/erl_db.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/erl_instrument.h beam/erl_mtrace.h beam/dist.h beam/erl_gc.h \
 beam/erl_cpu_topology.h hipe/hipe_arch.h hipe/hipe_x86.h
$(OBJDIR)/erl_bif_lists.o: beam/erl_bif_lists.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h
$(OBJDIR)/erl_bif_op.o: beam/erl_bif_op.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_binary.h
$(OBJDIR)/erl_bif_os.o: beam/erl_bif_os.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h
$(OBJDIR)/erl_bif_port.o: beam/erl_bif_port.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/erl_sys_driver.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_node_container_utils.h beam/hash.h beam/index.h beam/atom.h \
 i686-pc-linux-gnu/erl_atom_table.h beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_binary.h beam/erl_db_util.h \
 beam/packet_parser.h beam/erl_driver.h beam/erl_bits.h
$(OBJDIR)/erl_bif_re.o: beam/erl_bif_re.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_binary.h beam/big.h \
 pcre/pcre.h
$(OBJDIR)/erl_bif_timer.o: beam/erl_bif_timer.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_bif_timer.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_vm.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_monitors.h beam/erl_port_task.h beam/erl_sys_driver.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_time.h \
 i686-pc-linux-gnu/erl_atom_table.h hipe/hipe_process.h beam/erl_alloc.h \
 beam/export.h beam/index.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/global.h beam/atom.h \
 beam/module.h beam/register.h beam/erl_fun.h beam/benchmark.h \
 beam/erl_debug.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/error.h beam/big.h
$(OBJDIR)/erl_bif_trace.o: beam/erl_bif_trace.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h beam/beam_bp.h beam/erl_binary.h
$(OBJDIR)/erl_bits.o: beam/erl_bits.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_bits.h \
 beam/erl_binary.h
$(OBJDIR)/erl_cpu_topology.o: beam/erl_cpu_topology.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_cpu_topology.h
$(OBJDIR)/erl_db.o: beam/erl_db.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/big.h
$(OBJDIR)/erl_db_hash.o: beam/erl_db_hash.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/big.h \
 beam/erl_binary.h
$(OBJDIR)/erl_db_tree.o: beam/erl_db_tree.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/big.h \
 beam/erl_binary.h
$(OBJDIR)/erl_db_util.o: beam/erl_db_util.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/big.h \
 beam/erl_binary.h
$(OBJDIR)/erl_debug.o: beam/erl_debug.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_nmgc.h \
 beam/big.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/beam_catches.h
$(OBJDIR)/erl_drv_thread.o: beam/erl_drv_thread.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_fun.o: beam/erl_fun.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_gc.o: beam/erl_gc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_db.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_db_hash.h beam/erl_db_tree.h beam/beam_catches.h \
 beam/erl_binary.h beam/erl_bits.h beam/error.h beam/big.h beam/erl_gc.h \
 hipe/hipe_stack.h hipe/hipe_arch.h hipe/hipe_x86.h \
 hipe/hipe_mode_switch.h beam/error.h
$(OBJDIR)/erl_goodfit_alloc.o: beam/erl_goodfit_alloc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h \
 beam/erl_goodfit_alloc.h sys/common/erl_mseg.h beam/sys.h
$(OBJDIR)/erl_init.o: beam/erl_init.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 i686-pc-linux-gnu/erl_version.h beam/erl_db.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_db_util.h beam/erl_db_hash.h \
 beam/erl_db_tree.h beam/beam_bp.h beam/erl_bits.h beam/erl_binary.h \
 beam/dist.h sys/common/erl_mseg.h beam/sys.h beam/erl_nmgc.h \
 beam/erl_instrument.h beam/erl_mtrace.h beam/erl_printf_term.h \
 ../include/internal/erl_printf_format.h beam/packet_parser.h \
 beam/erl_driver.h beam/erl_cpu_topology.h hipe/hipe_mode_switch.h \
 beam/error.h hipe/hipe_signal.h
$(OBJDIR)/erl_instrument.o: beam/erl_instrument.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/big.h \
 beam/erl_instrument.h beam/erl_mtrace.h
$(OBJDIR)/erl_lock_check.o: beam/erl_lock_check.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/erl_lock_count.o: beam/erl_lock_count.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/erl_math.o: beam/erl_math.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h
$(OBJDIR)/erl_md5.o: beam/erl_md5.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_binary.h
$(OBJDIR)/erl_message.o: beam/erl_message.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_nmgc.h \
 beam/erl_binary.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h
$(OBJDIR)/erl_monitors.o: beam/erl_monitors.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h \
 beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/big.h
$(OBJDIR)/erl_mtrace.o: beam/erl_mtrace.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_sock.h \
 ../include/internal/erl_memory_trace_protocol.h beam/erl_mtrace.h
$(OBJDIR)/erl_nif.o: beam/erl_nif.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_nif.h beam/erl_drv_nif.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_binary.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/error.h beam/big.h \
 beam/beam_bp.h
$(OBJDIR)/erl_nmgc.o: beam/erl_nmgc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_gc.h \
 beam/erl_binary.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_nmgc.h hipe/hipe_stack.h hipe/hipe_arch.h hipe/hipe_x86.h
$(OBJDIR)/erl_node_tables.o: beam/erl_node_tables.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/dist.h beam/big.h \
 beam/error.h beam/erl_db.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h
$(OBJDIR)/erl_port_task.o: beam/erl_port_task.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/dist.h
$(OBJDIR)/erl_posix_str.o: beam/erl_posix_str.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 ../include/internal/erl_errno.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h
$(OBJDIR)/erl_printf_term.o: beam/erl_printf_term.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_printf_term.h ../include/internal/erl_printf_format.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/big.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_process.o: beam/erl_process.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_nmgc.h \
 beam/error.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/erl_db.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/dist.h \
 beam/beam_catches.h beam/erl_instrument.h beam/erl_mtrace.h \
 beam/erl_binary.h beam/beam_bp.h beam/erl_cpu_topology.h \
 hipe/hipe_mode_switch.h beam/error.h hipe/hipe_signal.h
$(OBJDIR)/erl_process_dict.o: beam/erl_process_dict.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 i686-pc-linux-gnu/erl_version.h
$(OBJDIR)/erl_process_dump.o: beam/erl_process_dump.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_db.h beam/erl_db_util.h \
 beam/erl_db_hash.h beam/erl_db_tree.h beam/dist.h beam/beam_catches.h \
 beam/erl_binary.h
$(OBJDIR)/erl_process_lock.o: beam/erl_process_lock.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_process.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_process_lock.h beam/erl_vm.h \
 beam/erl_message.h beam/external.h beam/erl_node_tables.h beam/hash.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h \
 beam/erl_bif_timer.h beam/erl_time.h i686-pc-linux-gnu/erl_atom_table.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/export.h beam/index.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h
$(OBJDIR)/erl_resolv_dns.o: beam/erl_resolv_dns.c
$(OBJDIR)/erl_resolv_nodns.o: beam/erl_resolv_nodns.c
$(OBJDIR)/erl_term.o: beam/erl_term.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_time_sup.o: beam/erl_time_sup.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_trace.o: beam/erl_trace.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/big.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/dist.h beam/beam_bp.h \
 beam/error.h beam/erl_binary.h beam/erl_bits.h
$(OBJDIR)/erl_unicode.o: beam/erl_unicode.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_binary.h beam/big.h \
 beam/erl_unicode.h beam/erl_unicode_normalize.h
$(OBJDIR)/erl_zlib.o: beam/erl_zlib.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_zlib.h zlib/zlib.h zlib/zconf.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h
$(OBJDIR)/export.o: beam/export.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/external.o: beam/external.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/dist.h \
 beam/erl_binary.h beam/erl_bits.h beam/erl_zlib.h zlib/zlib.h \
 zlib/zconf.h hipe/hipe_mode_switch.h beam/error.h
$(OBJDIR)/fix_alloc.o: beam/fix_alloc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_db.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_db_hash.h beam/erl_db_tree.h
$(OBJDIR)/hash.o: beam/hash.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/index.o: beam/index.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/io.o: beam/io.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/dist.h beam/big.h \
 beam/erl_binary.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_bits.h i686-pc-linux-gnu/erl_version.h beam/error.h
$(OBJDIR)/module.o: beam/module.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/packet_parser.o: beam/packet_parser.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/packet_parser.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/register.o: beam/register.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h
$(OBJDIR)/safe_hash.o: beam/safe_hash.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/safe_hash.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h
$(OBJDIR)/time.o: beam/time.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/utils.o: beam/utils.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_node_container_utils.h beam/hash.h \
 beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/big.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_binary.h beam/erl_bits.h \
 beam/packet_parser.h beam/erl_driver.h beam/erl_gc.h beam/erl_db.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h beam/dist.h
$(OBJDIR)/efile_drv.o: drivers/common/efile_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 drivers/common/erl_efile.h beam/erl_threads.h zlib/zlib.h zlib/zconf.h \
 drivers/common/gzio.h
$(OBJDIR)/gzio.o: drivers/common/gzio.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_smp.h beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h drivers/common/gzio_zutil.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h beam/erl_zlib.h zlib/zlib.h \
 drivers/common/gzio.h
$(OBJDIR)/inet_drv.o: drivers/common/inet_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_smp.h beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/packet_parser.h beam/erl_sock.h
$(OBJDIR)/ram_file_drv.o: drivers/common/ram_file_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 zlib/zlib.h zlib/zconf.h drivers/common/gzio.h
$(OBJDIR)/zlib_drv.o: drivers/common/zlib_drv.c zlib/zlib.h zlib/zconf.h \
 beam/erl_driver.h \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h
$(OBJDIR)/bin_drv.o: drivers/unix/bin_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h
$(OBJDIR)/multi_drv.o: drivers/unix/multi_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_smp.h beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/sig_drv.o: drivers/unix/sig_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h
$(OBJDIR)/ttsl_drv.o: drivers/unix/ttsl_drv.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_smp.h beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/unix_efile.o: drivers/unix/unix_efile.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 drivers/common/erl_efile.h
$(OBJDIR)/erl_child_setup.o: sys/unix/erl_child_setup.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h
$(OBJDIR)/erl_main.o: sys/unix/erl_main.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_unix_sys_ddll.o: sys/unix/erl_unix_sys_ddll.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/sys.o: sys/unix/sys.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_threads.h \
 sys/common/erl_mseg.h $(TTF_DIR)/erl_alloc_types.h \
 beam/global.h beam/erl_alloc.h beam/erl_alloc_util.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/hash.h beam/index.h beam/atom.h \
 i686-pc-linux-gnu/erl_atom_table.h beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/erl_sys_driver.h \
 sys/common/erl_check_io.h beam/erl_cpu_topology.h
$(OBJDIR)/sys_float.o: sys/unix/sys_float.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h
$(OBJDIR)/sys_time.o: sys/unix/sys_time.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_check_io.kp.o $(OBJDIR)/erl_check_io.nkp.o: sys/common/erl_check_io.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h \
 sys/common/erl_check_io.h beam/erl_sys_driver.h sys/common/erl_poll.h \
 beam/erl_port_task.h
$(OBJDIR)/erl_mseg.o: sys/common/erl_mseg.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h sys/common/erl_mseg.h \
 $(TTF_DIR)/erl_alloc_types.h beam/global.h \
 beam/erl_alloc.h beam/erl_alloc_util.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/hash.h beam/index.h beam/atom.h \
 i686-pc-linux-gnu/erl_atom_table.h beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_threads.h \
 beam/erl_mtrace.h beam/erl_time.h beam/big.h
$(OBJDIR)/erl_mtrace_sys_wrap.o: sys/common/erl_mtrace_sys_wrap.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_mtrace.h \
 $(TTF_DIR)/erl_alloc_types.h
$(OBJDIR)/erl_poll.kp.o $(OBJDIR)/erl_poll.nkp.o: sys/common/erl_poll.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 sys/common/erl_poll.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_smp.h beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h
$(OBJDIR)/erl_sys_common_misc.o: sys/common/erl_sys_common_misc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/erl_atom_table.o: i686-pc-linux-gnu/erl_atom_table.c
$(OBJDIR)/erl_bif_table.o: i686-pc-linux-gnu/erl_bif_table.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/index.h beam/hash.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_vm.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h \
 beam/erl_bif_timer.h beam/erl_time.h i686-pc-linux-gnu/erl_atom_table.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/export.h beam/sys.h \
 beam/erl_vm.h beam/erl_process.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h i686-pc-linux-gnu/erl_bif_table.h \
 i686-pc-linux-gnu/erl_atom_table.h
$(OBJDIR)/erl_bif_wrap.o: i686-pc-linux-gnu/erl_bif_wrap.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/index.h beam/hash.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_vm.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h \
 beam/erl_bif_timer.h beam/erl_time.h i686-pc-linux-gnu/erl_atom_table.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/export.h beam/sys.h \
 beam/erl_vm.h beam/global.h beam/atom.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 i686-pc-linux-gnu/erl_bif_table.h i686-pc-linux-gnu/erl_atom_table.h
$(OBJDIR)/erl_pbifs.o: i686-pc-linux-gnu/erl_pbifs.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/index.h beam/hash.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_vm.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h \
 beam/erl_bif_timer.h beam/erl_time.h i686-pc-linux-gnu/erl_atom_table.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/export.h beam/sys.h \
 beam/erl_vm.h beam/global.h beam/atom.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 i686-pc-linux-gnu/erl_bif_table.h i686-pc-linux-gnu/erl_atom_table.h
$(OBJDIR)/preload.o: i686-pc-linux-gnu/preload.c
$(OBJDIR)/beam_opcodes.o: $(TTF_DIR)/beam_opcodes.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/export.h \
 beam/index.h beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_vm.h \
 beam/erl_message.h beam/external.h beam/erl_node_tables.h \
 beam/erl_monitors.h beam/erl_port_task.h beam/erl_sys_driver.h \
 beam/erl_driver.h ../include/i686-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_drv_nif.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_bif_timer.h beam/erl_time.h \
 i686-pc-linux-gnu/erl_atom_table.h hipe/hipe_process.h beam/erl_alloc.h \
 beam/export.h beam/erl_process.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/beam_load.h
$(OBJDIR)/driver_tab.o: $(TTF_DIR)/driver_tab.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h
$(OBJDIR)/hipe_amd64.o: hipe/hipe_amd64.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_bif0.h hipe/hipe_native_bif.h \
 i686-pc-linux-gnu/erl_bif_list.h \
 $(TTF_DIR)/hipe_literals.h
$(OBJDIR)/hipe_arm.o: hipe/hipe_arm.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_binary.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_native_bif.h i686-pc-linux-gnu/erl_bif_list.h \
 hipe/hipe_bif0.h
$(OBJDIR)/hipe_bif0.o: hipe/hipe_bif0.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/error.h beam/erl_vm.h \
 beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/beam_load.h \
 beam/erl_db.h beam/bif.h beam/erl_db_util.h beam/global.h \
 beam/erl_db_hash.h beam/erl_db_tree.h beam/hash.h beam/erl_bits.h \
 beam/erl_binary.h hipe/hipe_arch.h hipe/hipe_x86.h hipe/hipe_stack.h \
 hipe/hipe_mode_switch.h hipe/hipe_native_bif.h \
 i686-pc-linux-gnu/erl_bif_list.h hipe/hipe_bif0.h \
 $(TTF_DIR)/hipe_literals.h hipe/hipe_primops.h \
 hipe/hipe_x86_primops.h
$(OBJDIR)/hipe_bif1.o: hipe/hipe_bif1.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/error.h \
 beam/beam_load.h hipe/hipe_bif0.h hipe/hipe_bif1.h
$(OBJDIR)/hipe_bif2.o: hipe/hipe_bif2.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/error.h beam/erl_vm.h \
 beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h beam/big.h \
 hipe/hipe_debug.h hipe/hipe_mode_switch.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_stack.h
$(OBJDIR)/hipe_bif64.o: hipe/hipe_bif64.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/error.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_bif0.h hipe/hipe_bif64.h
$(OBJDIR)/hipe_debug.o: hipe/hipe_debug.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/beam_catches.h beam/beam_load.h hipe/hipe_mode_switch.h \
 beam/error.h hipe/hipe_debug.h
$(OBJDIR)/hipe_gc.o: hipe/hipe_gc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_gc.h \
 hipe/hipe_stack.h hipe/hipe_arch.h hipe/hipe_x86.h hipe/hipe_gc.h \
 hipe/hipe_x86_gc.h $(TTF_DIR)/hipe_x86_asm.h
$(OBJDIR)/hipe_mkliterals.o: hipe/hipe_mkliterals.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/error.h beam/erl_bits.h beam/erl_message.h \
 $(TTF_DIR)/hipe_arm_asm.h \
 $(TTF_DIR)/hipe_ppc_asm.h \
 $(TTF_DIR)/hipe_amd64_asm.h \
 $(TTF_DIR)/hipe_x86_asm.h \
 $(TTF_DIR)/hipe_sparc_asm.h beam/erl_binary.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h
$(OBJDIR)/hipe_mode_switch.o: hipe/hipe_mode_switch.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/beam_load.h beam/beam_catches.h hipe/hipe_mode_switch.h \
 beam/error.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 hipe/hipe_stack.h hipe/hipe_arch.h hipe/hipe_x86.h hipe/hipe_bif0.h \
 hipe/hipe_x86_glue.h $(TTF_DIR)/hipe_x86_asm.h
$(OBJDIR)/hipe_native_bif.o: hipe/hipe_native_bif.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_process.h \
 beam/error.h beam/bif.h i686-pc-linux-gnu/erl_bif_table.h \
 beam/erl_bits.h beam/erl_binary.h beam/bif.h hipe/hipe_mode_switch.h \
 hipe/hipe_native_bif.h hipe/hipe_arch.h hipe/hipe_x86.h \
 i686-pc-linux-gnu/erl_bif_list.h hipe/hipe_stack.h
$(OBJDIR)/hipe_perfctr.o: hipe/hipe_perfctr.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_smp.h beam/erl_threads.h \
 ../include/internal/ethread.h ../include/internal/i386/ethread.h \
 ../include/internal/i386/atomic.h ../include/internal/i386/spinlock.h \
 ../include/internal/i386/rwlock.h ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/error.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h beam/erl_vm.h beam/erl_node_container_utils.h \
 beam/hash.h beam/index.h beam/atom.h i686-pc-linux-gnu/erl_atom_table.h \
 beam/export.h beam/beam_load.h \
 $(TTF_DIR)/beam_opcodes.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_monitors.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h beam/big.h beam/erl_binary.h \
 beam/bif.h hipe/hipe_perfctr.h libperfctr.h
$(OBJDIR)/hipe_ppc.o: hipe/hipe_ppc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/erl_binary.h \
 beam/bif.h i686-pc-linux-gnu/erl_bif_table.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_native_bif.h i686-pc-linux-gnu/erl_bif_list.h \
 hipe/hipe_bif0.h
$(OBJDIR)/hipe_risc_stack.o: hipe/hipe_risc_stack.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h hipe/hipe_stack.h hipe/hipe_arch.h \
 hipe/hipe_x86.h
$(OBJDIR)/hipe_sparc.o: hipe/hipe_sparc.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_native_bif.h i686-pc-linux-gnu/erl_bif_list.h
$(OBJDIR)/hipe_stack.o: hipe/hipe_stack.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h hipe/hipe_stack.h \
 hipe/hipe_arch.h hipe/hipe_x86.h
$(OBJDIR)/hipe_x86.o: hipe/hipe_x86.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h hipe/hipe_arch.h \
 hipe/hipe_x86.h hipe/hipe_native_bif.h i686-pc-linux-gnu/erl_bif_list.h \
 $(TTF_DIR)/hipe_literals.h
$(OBJDIR)/hipe_x86_signal.o: hipe/hipe_x86_signal.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 hipe/hipe_signal.h
$(OBJDIR)/hipe_x86_stack.o: hipe/hipe_x86_stack.c \
 $(ERL_TOP)/erts/i686-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_smp.h \
 beam/erl_threads.h ../include/internal/ethread.h \
 ../include/internal/i386/ethread.h ../include/internal/i386/atomic.h \
 ../include/internal/i386/spinlock.h ../include/internal/i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/libatomic_ops/ethr_atomic.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_term.h \
 ../include/internal/erl_printf.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_alloc_util.h \
 beam/erl_vm.h beam/erl_node_container_utils.h beam/hash.h beam/index.h \
 beam/atom.h i686-pc-linux-gnu/erl_atom_table.h beam/export.h \
 beam/beam_load.h $(TTF_DIR)/beam_opcodes.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_monitors.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 ../include/i686-pc-linux-gnu/erl_int_sizes_config.h beam/erl_drv_nif.h \
 beam/erl_process_dict.h beam/erl_bif_timer.h beam/erl_time.h \
 hipe/hipe_process.h beam/erl_alloc.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/benchmark.h beam/erl_debug.h beam/bif.h \
 i686-pc-linux-gnu/erl_bif_table.h hipe/hipe_stack.h hipe/hipe_arch.h \
 hipe/hipe_x86.h $(TTF_DIR)/hipe_x86_asm.h
