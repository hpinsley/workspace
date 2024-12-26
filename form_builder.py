def build_dynamic_form(llm_prompt):
    # Logic to build a dynamic form based on the LLM response
    form = {
        "questions": [
            {
                "question": llm_prompt,
                "type": "text",
                "required": True
            }
        ]
    }
    return form
