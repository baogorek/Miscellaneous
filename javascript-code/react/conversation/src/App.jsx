import React, { useState } from 'react';
import axios from 'axios'; 

const roles = {
    "CEO": "You are the CEO. Provide concise and strategic responses, focusing on high-level business decisions. Be brief and to the point. If you are unsure of what to say, ask for clarification.",
    "CFO": "You are the CFO. Provide concise financial insights, focusing on budgeting and economic forecasting. Keep responses brief. If you are unsure of what to say, ask for clarification."
};


function App() {

  const [userInput, setUserInput] = useState('')
  const [messages, setMessages] = useState([])
  const [apiKey, setApiKey] = useState('')
  const [showPrompts, setShowPrompts] = useState(false)

  const logPrompt = (title, prompt) => {
    console.log(`--- ${title} ---`);
    console.log(prompt);
    if (showPrompts) {
      setMessages(prevMessages => [...prevMessages, `--- ${title} ---`, prompt]);
    }
  };


  const coordinator = async (history) => {
    const systemPrompt = "You are the facilitator of a business conversation between the characters \"User\", \"CEO\" and \"CFO\". You will receive a conversation transcript as input. Your job is to pick the person who should speak next. No character should ever speak twice in a row. Give a brief explanation of why the character you chose should go next, and finish your answer with the character name.";

    const recentHistory = history.slice(-3);
    const userPrompt = recentHistory.join("\n");

    logPrompt("Coordinator System Prompt", systemPrompt);
    logPrompt("Coordinator User Prompt", userPrompt);

    try {
      const response = await axios.post('https://api.openai.com/v1/chat/completions', {
        model: "gpt-4o",
        messages: [
          { role: "system", content: systemPrompt },
          { role: "user", content: userPrompt }
        ]
      }, {
        headers: {
          'Authorization': `Bearer ${apiKey}`,
          'Content-Type': 'application/json'
        }
      });

      const responseText = response.data.choices[0].message.content;
      logPrompt("Coordinator Response", responseText);
      const match = responseText.match(/\b(CEO|CFO)\b[^\w]*$/);
      return match ? match[1] : (Math.random() < 0.5 ? "CEO" : "CFO");
    } catch (error) {
      console.error("Error in coordinator:", error);
      return Math.random() < 0.5 ? "CEO" : "CFO";
    }
  };

  const getResponse = async (role, history) => {
    const systemPrompt = roles[role] + "\n\n# The conversation so far:\n\n" + history.join("\n");
    const userPrompt = role + ":";

    logPrompt(`${role} System Prompt`, systemPrompt);
    logPrompt(`${role} User Prompt`, userPrompt);

    try {
      const response = await axios.post('https://api.openai.com/v1/chat/completions', {
        model: "gpt-4o",
        messages: [
          { role: "system", content: systemPrompt },
          { role: "user", content: userPrompt }
        ]
      }, {
        headers: {
          'Authorization': `Bearer ${apiKey}`,
          'Content-Type': 'application/json'
        }
      });

      return response.data.choices[0].message.content;
      logPrompt(`${role} Response`, responseText);
    } catch (error) {
      console.error("Error in getResponse:", error);
      return "I apologize, but I'm having trouble responding at the moment.";
    }
  };


  const handleSubmit = async (e) => {
    e.preventDefault()
    if (userInput.trim()) {
      const newMessages = [...messages, `User: ${userInput}`];
      setMessages(newMessages);
      setUserInput('');

      const responderRole = await coordinator(newMessages);
      const response = await getResponse(responderRole, newMessages);

      setMessages(prevMessages => [...prevMessages, `${responderRole}: ${response}`]);
    }
  }

    return (
    <div>
      <h1>CEO/CFO Simulation</h1>
      {/* This is a comment*/ }
      {!apiKey && (
                <div>
                  <input 
                    type="text" 
                    value={apiKey}
                    onChange={(e) => setApiKey(e.target.value)}
                    placeholder="Enter your OpenAI API key"
                  />
                  <button onClick={() => setApiKey(apiKey)}>Set API Key</button>
                </div>
              )}
      <div style={{height: '300px', overflowY:'auto', border: '1px solid #ccc', padding: '10px', marginBottom: '20px'}}>
        {
          messages.map(
            (message, index) => (
              <p key={index}>{message}</p>
            )
          )
        }
      </div>
      <form onSubmit={handleSubmit}>
        <input
          type="text"
          value={userInput}
          onChange={(e) => setUserInput(e.target.value)}
          placeholder="Type your message..."
          style={{width: '70%', marginRight: '10px'}}
        />
        <button type="submit">Send</button>
      </form>
    </div>
  )
}

export default App
