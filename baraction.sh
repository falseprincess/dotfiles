#!/bin/bash
# Requires: acpi, iostat, lm-sensors, aptitude.
# Tested on: Debian Testing
# This script can be found at 


############################
#          NET
############################

net() { 
net="$(/home/sudozelda/bin/netspeed)"
echo -e "NET: $net" 
}

#############################
#         BATTERY
#############################

bat() {
	power=$(cat /sys/class/power_supply/BAT0/capacity)
	echo -e "BAT: $power%"
}

############################## 
#	    DATE
##############################

date() {
	  date="$(date +"%r")"
	    echo -e " $dte "
    }

############################## 
#	    DISK
##############################

hdd() {
	  hdd="$(df -h /home | grep /dev | awk '{print $3 " / " $5}')"
	    echo -e "HDD: $hdd"
    }

##############################
#	    RAM
##############################

mem() {
	mem="$(free -h | awk '/Mem:/ {printf $3 "/" $2}')"
	echo -e "MEM: $mem"
}

##############################	
#	    CPU
##############################

cpu() {
	  read cpu a b c previdle rest < /proc/stat
	    prevtotal=$((a+b+c+previdle))
	      sleep 0.5
	        read cpu a b c idle rest < /proc/stat
		  total=$((a+b+c+idle))
		    cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
		      echo -e "CPU: $cpu%"
	      }

##############################
#	    VOLUME
##############################

vol() {
	vol="$(amixer -D pulse get Master | awk -F'[][]' 'END{ print $4":"$2 }')"
	echo -e "VOL: $vol"
}

##############################
#	    VPN
##############################

vpn() {
	vpn="$(ip a | grep tun0 | grep inet | wc -l)"
	echo -e "$vpn"
}

## TEMP
temp() {
	tmp="$(grep temp_F ~/.config/weather.txt | awk '{print $2}' | sed 's/"//g' | sed 's/,/ F/g')"
	echo "TEM: $tmp"
}


      SLEEP_SEC=1
      #loops forever outputting a line every SLEEP_SEC secs
      while :; do     
	      echo "+@fg=1;$(hdd)+@fg=0; | +@fg=3;$(cpu)+@fg=0; | +@fg=5;$(net)+@fg=0; | +@fg=4;$(mem)+@fg=0; | +@fg=2;VPN $(vpn)+@fg=0; | +@fg=6;$(vol)+@fg=0; | +@fg=7;$(bat)+@fg=0;"
   sleep $SLEEP_SEC
		done
