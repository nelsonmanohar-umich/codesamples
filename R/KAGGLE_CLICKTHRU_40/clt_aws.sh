#! /bin/bash
### BEGIN INIT INFO
# Provides:          skeleton
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: clt_aws_compute_node
# Description:       This file should be used to construct scripts to be
#                    placed in /etc/init.d.
### END INIT INFO

CLT_USER=ubuntu
CLT_DIR=/home/${CLT_USER}/WORKSPACE/R/
CLT_AWS_CLIENT=${CLT_DIR}/clt_aws_client.py

case "$1" in
 start)
   for i in `seq 1 50` ; do sudo -u ubuntu -g ubuntu -H /usr/bin/python ${CLT_AWS_CLIENT} ; done
   clt_pid=`/bin/ps -fe | /bin/grep clt_aws | /bin/grep WORKSPACE | /bin/grep R | /bin/grep python | /usr/bin/awk '{ print $2 }'`
   /bin/echo ${clt_pid} 
   ;;
 stop)
   clt_pid=`/bin/ps -fe | /bin/grep clt_aws | /bin/grep WORKSPACE | /bin/grep R | /bin/grep python | /usr/bin/awk '{ print $2 }'`
   /bin/echo "--------------"
   /bin/echo `/bin/pidof python` 
   /bin/echo `/usr/bin/pgrep python`
   /bin/echo "--------------"
   /bin/echo ${clt_pid}
   /bin/echo "--------------"
   /bin/kill -9 ${clt_pid}
   /bin/sleep 1
   /bin/echo "--------------"
   ;;
 restart)
   clt_pid=`/bin/ps -fe | /bin/grep clt_aws | /bin/grep WORKSPACE | /bin/grep R | /bin/grep python | /usr/bin/awk '{ print $2 }'`
   /bin/echo "--------------"
   /bin/echo `/bin/pidof python` 
   /bin/echo `/usr/bin/pgrep python`
   /bin/echo "--------------"
   /bin/echo ${clt_pid}
   /bin/echo "--------------"
   /bin/kill -9 ${clt_pid}
   /bin/sleep 1
   /bin/echo "--------------"
   for i in `seq 1 50` ; do sudo -u ubuntu -g ubuntu -H /usr/bin/python ${CLT_AWS_CLIENT} ; done
   ;;
 *)
   echo "Usage: clt_aws {start|stop|restart}" >&2
   exit 3
   ;;
esac
