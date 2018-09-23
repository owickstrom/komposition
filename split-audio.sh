set -e

rm -r output
mkdir -p output

echo "Normalizing recording ..."
sox -S --norm haw-0009-voiceover.mp3 output/complete.mp3
echo "Splitting recording into chunks ..."
sox -S output/complete.mp3 output/chunk.mp3 silence 1 0 0.05% 1 0.5 0.05% : newfile : restart

echo "Removing silent files ..."
for file in output/chunk*.mp3; do
  maximum_amplitude=$(sox "$file" -n stat 2>&1 | grep 'Maximum amplitude' | tr -s ' ' | cut -d ' ' -f 3)
  if (( $(echo "$maximum_amplitude>0.05" | bc -l) )); then
    continue
  else
    rm "$file"
  fi
done

echo "Done!"
vlc output/chunk*.mp3 &
