import openai
import random


roles = {
    "CEO": "You are the CEO. Provide concise and strategic responses, focusing on high-level business decisions. Be brief and to the point. If you are unsure of what to say, ask for clarification.",
    "CFO": "You are the CFO. Provide concise financial insights, focusing on budgeting and economic forecasting. Keep responses brief. If you are unsure of what to say, ask for clarification."
}

def coordinator(prompt):
    return random.choice(["CEO", "CFO"])


def get_response(role, prompt):
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
    print("Welcome to the CEO/CFO simulation.")
    print("Type 'exit' to end the conversation.\n")

    openai.api_key = input("Enter your OpenAI key: ")
    print("Key received. Start the conversation when you are ready!.\n")

    while True:
        user_input = input("You: ")

        if user_input.lower() == "exit":
            print("Goodbye!")
            break

        responder = coordinator(user_input)
        response = get_response(responder, user_input)
        
        print(f"\n{responder}: {response}\n")

if __name__ == "__main__":
    main()

