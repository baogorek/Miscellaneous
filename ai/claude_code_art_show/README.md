The Claude Code ASCII Art Show!
========================================

```
● Artist 1: RZR                                                                                                                   
                                                                                                                                  
  Artwork: "TOKYO GHOST PROTOCOL v2.█"                                                                                            
                                                                                                                                  
                  _____      ______    ____                                                                                       
             /\     \    /\  _  \  /\   \_\   TOKYO GHOST PROTOCOL v2.█                                                           
            /  \     \  /  / \  \/  /\   \ \  ------------------------------                                                      
           /    \     \/   \__\    /  \   \ \ [SYSTEM CORRUPTION IMMINENT]                                                        
          /      \    /\         /\   \   \ \                                                                                     
         /   /\   \  /  \       /  \   \   \ \   .:*GLITCH SEQUENCE ACTIVE*:.                                                     
        /   /  \   \/    \     /    \   \   \ \                                                                                   
       /   /    \   \     \   /      \   \   \ \                                                                                  
      /   /    / \   \     \ /        \   \   \ \
     /   /    /   \   \     \          \   \   \ \
    /___/____/     \___\     \          \___\   \ \
   |    |    |     |    |     \          |    |   \ \
   |____|____|_____|____|______\         |____|    \_\
    ████████████████████████████         ██████████

     /\     _____      ___    ___     /\     _____
    /  \   /\    \    /\  \  /\  \   /  \   /\    \
   /    \ /  \    \  /  \  \/  \  \ /    \ /  \    \
  /      \\   \    \/    \      /  //      \\   \    \
  /   /\   \\   \          \    /  //   /\   \\   \    \
  \___\/\___\\_______\______\__/  / \___\/\___\\_______\
   \    \    \|      ||      |   /   \    \    \|      |
    \____\____\      ||      |__/     \____\____\      |
     \/   \/   |_____||_____/  \/       \/   \/   |____|

    [CORRUPTION LEVEL: ███████████████ 95% ]
    
    - RZR

  Critic's Evaluation:

  TOTAL SCORE: 75/100 (2nd Place)

  Summary: RZR's cyberpunk masterpiece showcased excellent thematic coherence with innovative narrative integration. The
  piece scored particularly high in Creativity & Originality (22/25) for its bold corruption theme and atmospheric text
  elements. While some technical inconsistencies were noted in line alignment, the overall impact delivered a strong
  cyberpunk identity with clear visual hierarchy and effective use of progress bars as artistic elements.

```
See it on [YouTube](https://www.youtube.com/watch?v=d08tZ4BsjlY).

## Subagents in Action

This is a silly but quite complete example of Claude Code subagents in action.
There are three "artists" hat make ASCII art, and these are Claude Code subagents:

- RZR
    - .claude/agents/ascii-artist-rzr.md
    - Cyberpunk glitch style with jagged, aggressive urban graffiti aesthetics and deliberate digital decay

- Vivienne 'Pixel Prophet' Cross
    - .claude/agents/ascii-artist-vivienne.md 
    - Geometric mandalas and sacred geometry using only punctuation marks
  and numbers, perfectly symmetrical

- Mad Max 'Terminal' Rodriguez 
    - .claude/agents/mad-max-ascii-artist.md
    - Detailed explosive vehicle designs (motorcycles, tanks, spaceships) with hidden skull signatures


## Running the example 

You'll need Claude Code installed. Open it in a terminal while in this directory so that it reads the CLAUDE.md file.
If you peak into this file, you'll see a brief explaining of the ASCII art show that is about to
happen. The three "artist" agents and the critic are referenced with their @ symbols. The flow of the program
is explained, including sequential vs parallel execution

Since this demo is very low risk, the recommended way to execute the demo is to run:
```
claude --dangerously-skip-permissions 
```
And then start the conversation with:

```
> Let's get this show on the road!
```

It can be a little difficult to see what's happening in real time, though you can press CTRL + R to try.
Afterwards, you can check out the text files that were provided for a full trace.

Alternately, you can try turning on the verbose flag:

```
claude --dangerously-skip-permissions --verbose
```
But I found this very jumpy.

Finally, if you want to have a full JSON log of the agent / subagent flow, this seems to work:
```
echo "Let's get this show on the road!" | claude --dangerously-skip-permissions --verbose --output-format stream-json | tee log.json
```

## Additional Commands

After the art show has run, you can view individual artist submissions with their critic scores:

```
/show-artist 1   # Shows RZR's artwork and critic evaluation
/show-artist 2   # Shows Vivienne's artwork and critic evaluation  
/show-artist 3   # Shows Mad Max's artwork and critic evaluation
```

This command will display:
- The artist's ASCII artwork from their .txt file
- Their total score from the critic
- A summary of the critic's final verdict

## Unsolved

 - I still haven't figured out how to make an agent call another agent.
 - It would be nice to have a log of the interactions without having to tee output
