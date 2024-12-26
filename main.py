import json
from llm_helper import generate_llm_prompt
from form_builder import build_dynamic_form

def main():
    user_prompt = input("Enter your objective: ")
    
    llm_prompt = generate_llm_prompt(user_prompt)
    
    form = build_dynamic_form(llm_prompt)
    
    print(json.dumps(form, indent=4))

if __name__ == "__main__":
    main()
