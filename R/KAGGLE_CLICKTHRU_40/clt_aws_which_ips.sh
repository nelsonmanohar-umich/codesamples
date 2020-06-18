echo
grep WHOAMI clt_aws_server.log | grep -v CMD | awk -F':' '{print $1 }' | sort | uniq -c
echo
grep S3Connection clt_aws_server.log | awk '{print $3}' | awk -F'-' '{print $1}' | sort | uniq -c
