import sijuda

while True:
    input_text = input('SIJUDA > ')
    if input_text.strip() == "": continue
    result, error = sijuda.run('<stdin>', input_text)
    if error:
        print(error.to_string())
    elif result:
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
        else:
            print(repr(result))
