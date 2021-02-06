#!/bin/sh
ACTUAL=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}')
case $ACTUAL in
	0)
	echo "[web] term chat media virt games"
	;;
        1)
        echo "web [term] chat media virt games"
 	;;
        2)
        echo "web term [chat] media virt games"
        ;;
        3)
        echo "web term chat [media] virt games"
        ;;
        4)
	echo "web term chat media [virt] games"
	;;
	5)
	echo "web term chat media virt [games]"
	;;
esac
