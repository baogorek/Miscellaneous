import openai
import random
import streamlit as st

# Define roles for the CEO and CFO
roles = {
    "CEO": "You are the CEO. Provide concise and strategic responses, focusing on high-level business decisions. Be brief and to the point. If you are unsure of what to say, ask for clarification.",
    "CFO": "You are the CFO. Provide concise financial insights, focusing on budgeting and economic forecasting. Keep responses brief. If you are unsure of what to say, ask for clarification."
}

def coordinator(prompt):
    return random.choice(["CEO", "CFO"])

def get_response(role, prompt, api_key):
    openai.api_key = api_key
    response = openai.chat.completions.create(
        model="gpt-4o",
        temperature=1,
        messages=[
            {"role": "system", "content": roles[role]},
            {"role": "user", "content": prompt}
        ]
    )
    return response.choices[0].message.content

def main():
    st.title("CEO/CFO Simulation")

    # Prompt for the OpenAI API key
    api_key = st.text_input("Enter your OpenAI API key:", type="password")

    # Initialize conversation history
    if "conversation" not in st.session_state:
        st.session_state.conversation = []

    # Display conversation history
    for message in st.session_state.conversation:
        st.markdown(f"**{message['role']}**: {message['content']}")

    # Input area for user's question
    if api_key:
        with st.form(key="user_input_form", clear_on_submit=True):
            user_input = st.text_input("You:")
            submit_button = st.form_submit_button(label="Submit")

        if submit_button and user_input:
            responder = coordinator(user_input)
            response = get_response(responder, user_input, api_key)

            # Update the conversation history
            st.session_state.conversation.append({"role": "You", "content": user_input})
            st.session_state.conversation.append({"role": responder, "content": response})

            # Re-run the app to display the updated conversation history and new input form
            st.experimental_rerun()

if __name__ == "__main__":
    main()

