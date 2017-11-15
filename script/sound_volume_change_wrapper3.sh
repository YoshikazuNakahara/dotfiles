case "$1" in
  "+")
    amixer pulse set Master 10%+ && aplay -D plughw:1  Musics/sound/test.wav & ;;
  "-")
    amixer pulse set Master 10%- && aplay -D plughw:1 Musics/sound/test.wav & ;;
  "m")
    amixer pulse set Master toggle ;;
esac
