#!/bin/sh
#=#=#=
# Take screenshot (full window) and save it to daydir(%Y-%m-%d).
#
# You may change the variable below.
#
# ```
# rootdir=$HOME/Workspace/blog
# ```
#
# >[ShellScript - スクリーンショットを撮って日付のディレクトリに連番で保存するスクリプト - Qiita](http://qiita.com/ssh0/items/2b4e7a4146cb2da01187)
#
#=#=

get_dir=false
get_name=false
rootdir=$HOME/Pictures

while getopts d:o:h OPT
do
  case $OPT in
    "d" ) get_dir=true
          dir="$OPTARG" ;;
    "o" ) get_name=true
          name="$OPTARG" ;;
    "h" ) usage_all "$0"
          exit 0 ;;
      * ) exit 1 ;;
  esac
done

# スクリーンショットを保存するディレクトリを設定
if ! $get_dir; then
    if [ ! -e $rootdir ]; then
        echo "There is no directory named: $rootdir"
        exit 1
    fi
    daydir=`date +%Y-%m-%d`
    dir=$rootdir/$daydir
fi

# ファイル名を設定
if ! $get_name; then
    if [ ! -e $dir ]; then
        mkdir $dir
        i=1
    else
        i=`expr $(ls $dir | sed -n 's/screen_\([0-9]\{3\}\).png/\1/p' | tail -n 1) + 1`
    fi
    name=$(printf screen_%03d.png $i)
fi

import -window root -quality 0 $dir/$name &
notify-send "Screenshot has been made" "saved: $dir/$name" &
exit 0
