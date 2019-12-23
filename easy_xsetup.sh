sudo service ssh --full-restart
export DISPLAY=localhost:10.0
ifconfig | grep 'inet '
echo "Start Putty with the non-localhost IP"
echo "And don't forget to start Xming"


