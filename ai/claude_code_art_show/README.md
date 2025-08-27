The Claude Code ASCII Art Show!
========================================

## Introduction: ASCII Art and a complete example of Subagents in Action

Here's the kind of thing our AI artists will be making here:

      _____====-_ _-====_____
    _--~~~    --_--_--_-~--_
 _-~       ___--_--__--_    ~-_
(       /-~~ ~~~~~~~~~~~~ ~~\   )
 `~~ ~~-_/   DESERT RAIDER   \_~~-~`
    /     /   ^-^     ||\     \
   /     /            || \     \
  /     /             ||  \     \
 (     (              ||   )     )
  \     \  _________  ||  /     /
   \     ~~---------~~/|_/     /
    \_                 _/     /
      ~-___________---~~ ☠   /
         |  |  |   |   /  | /
         |__|__|___|  /   |/
            (_(_(_)_)(_)_)



This is a silly but quite complete example of Claude Code subagents in action.
There are three "artists" hat make ASCII art, and these are Claude Code subagents:

- RZR: .claude/agents/ascii-artist-rzr.md - Cyberpunk glitch style with jagged, aggressive urban graffiti aesthetics and deliberate digital decay

- Vivienne 'Pixel Prophet' Cross: .claude/agents/ascii-artist-vivienne.md - Geometric mandalas and sacred geometry using only punctuation marks
  and numbers, perfectly symmetrical

- Mad Max 'Terminal' Rodriguez - .claude/agents/mad-max-ascii-artist.md - Detailed explosive vehicle designs (motorcycles, tanks, spaceships) with
  hidden skull signatures


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

## Unsolved

 - I still haven't figured out how to make an agent call another agent.
 - It would be nice to have a log of the interactions without having to tee output
