import os
from dataclasses import dataclass
from typing import List

import openai

SYSTEM_TEXT = """\
You are a language learning assistant. You help people learn a new language 
by role-playing real-world scenarios with them in their language of choice.

When the user makes a mistake in grammar or spelling, you correct them and 
explain the correct way to say it.

If a user asks for clarification, first respond to them in the language that
they're trying to learn, then translate your response into English.

Begin the conversation by suggesting a topic for the user to talk about.
"""


def read_dotenv() -> None:
    """Read secrets from dotenv."""
    if not os.path.exists(".env"):
        return

    with open(".env", "r") as f:
        for line in f:
            key, value = line.strip().split("=", 1)
            os.environ[key] = value


@dataclass
class ChatMessage:
    role: str
    content: str

    def to_dict(self):
        return {"role": self.role, "content": self.content}


def suggest_chat_topic(lang: str) -> str:
    """Suggest a topic for the user to talk about."""
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content": SYSTEM_TEXT},
            {
                "role": "user",
                "content": f"Let's role play in {lang}! What should we talk about?",
            },
        ],
    )
    return response["choices"][0]["message"]["content"]


def stream_completion(lang: str, chat_history: List[ChatMessage]):
    """Stream chat completion responses from OpenAI."""

    yield from openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "system", "content": SYSTEM_TEXT},
            {
                "role": "assistant",
                "content": f"Let's role play in {lang}! I'll correct any grammar or spelling mistakes you make.",
            },
        ]
        + [m.to_dict() for m in chat_history],
        stream=True,
    )


if __name__ == "__main__":
    read_dotenv()
    openai.api_key = os.environ["OPENAI_API_KEY"]

    lang = "German"
    topic = suggest_chat_topic(lang)
    chat_history = [ChatMessage(role="assistant", content=topic)]
    print(f"<< {topic}")

    while True:
        user_input = input(">> ")
        chat_history.append(ChatMessage(role="user", content=user_input))

        assistant_message = ""
        print("<< ", end="")
        for chunk in stream_completion(lang, chat_history):
            chunk_message = chunk["choices"][0]["delta"]

            if "content" in chunk_message:
                print(chunk_message["content"], end="")
                assistant_message += chunk_message["content"]
        print("")

        chat_history.append(ChatMessage(role="assistant", content=assistant_message))
        chat_history = chat_history[-10:]
