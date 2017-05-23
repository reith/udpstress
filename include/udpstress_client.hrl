-record(state, {socket, remote_addr, remote_port, sent_pkts=0, recv_pkts=0,
                sent_size=0, recv_size=0, not_acked_pkts=0, send_interval}).
