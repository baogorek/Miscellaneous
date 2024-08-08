import re
import random

import openai

# Logging structures

coordinator_prompts = []
coordinator_replies = []
system_prompts = []
user_prompts = []

# Program begins ---

roles = {
    "CEO": "You are the CEO. Provide concise and strategic responses, focusing on high-level business decisions. Be brief and to the point. If you are unsure of what to say, ask for clarification.",
    "CFO": "You are the CFO. Provide concise financial insights, focusing on budgeting and economic forecasting. Keep responses brief. If you are unsure of what to say, ask for clarification."
}


def extract_role_from_response(response):
    match = re.search(r'\b(CEO|CFO)\b[^\w]*$', response)
    if match:
        return match.group(1)
    return None


def coordinator(history):
    if len(history) == 0:
        raise ValueError("history should not be empty for the coordinator")

    system_prompt = "You are the faciliator of a business conversation between the characters \"User\", \"CEO\" and \"CFO\". You will recieve a conversation transcript as input. Your job is to pick the person who should speak next. No character should ever speak twice in a row. Give a brief explanation of why the character you chose should go next, and finish your answer with the character name."
    user_prompt = "\n".join(history)
    coordinator_prompts.append(user_prompt)
    response = openai.chat.completions.create(
        model="gpt-4o",
        temperature=1,
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]
    )
    response_text = response.choices[0].message.content
    coordinator_replies.append(response_text)
    role = extract_role_from_response(response_text)
    if role not in ['CEO', 'CFO']:
        print("couldn't extract a role. Randomly choosing. Response text was:")
        print(response_text)
        print("end response text")
        return random.choice(["CEO", "CFO"])
    return role


def create_system_prompt(role, history):
    role_prompt = roles[role]

    if len(history) == 0:
        return role_prompt

    history_prompt = "\n".join(history) 

    system_prompt = (
        role_prompt +
        "\n\n# The conversation so far:\n\n" +
        history_prompt
    )
    return system_prompt


def get_response(role, history):
    system_prompt = create_system_prompt(role, history)  # Thinking that this includes User questions now
    user_prompt = role + ":"
    system_prompts.append(system_prompt)
    user_prompts.append(user_prompt)
    response = openai.chat.completions.create(
        model="gpt-4o",
        temperature=1,
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]
    )
    return response.choices[0].message.content


def main():
    print("Welcome to the CEO/CFO simulation.")
    print("Type 'exit' to end the conversation.\n")

    openai.api_key = input("Enter your OpenAI key: ")
    print("Key received. Start the conversation when you are ready!.\n")

    conversation_history = []

    while True:
        user_input = input("You: ")

        if user_input.lower() == "exit":
            print("Goodbye!")
            break

        conversation_history.append(f"User: {user_input}")
        responder_role = coordinator(conversation_history)
        response = get_response(responder_role, conversation_history)

        conversation_history.append(f"{responder_role}: {response}")
        
        print(f"\n{responder_role}: {response}\n")


if __name__ == "__main__":
    main()
