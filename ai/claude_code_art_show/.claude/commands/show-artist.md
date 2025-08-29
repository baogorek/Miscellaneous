---
name: show-artist
description: Display an artist's artwork and critic score
arguments:
  - name: number
    description: Artist number (1=RZR, 2=Vivienne, 3=Mad Max)
    required: true
---

Display the artwork and critic evaluation for artist {{number}}.

Artist mapping:
- 1: RZR (file: rzr_artwork.txt)
- 2: Vivienne (file: vivienne_artwork.txt)  
- 3: Mad Max (file: mad_max_artwork.txt)

Steps:
1. First, validate that {{number}} is 1, 2, or 3
2. Display the artist's name
3. Read and display the corresponding artwork file
4. If critic.txt exists, search for that artist's evaluation and display:
   - The TOTAL SCORE line
   - A summary of the Final Verdict section