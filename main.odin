package parser
import "core:fmt"

main :: proc() {

	t := init_tokenizer("./test.freyja")
	line_no := 1
	for {
		token := scan(&t)
		if token.kind == .EOF do break
		if token.pos.line > line_no {
			fmt.printf("\n")
			line_no = token.pos.line
		}
		fmt.printf("%v (%v) ", token.kind, token.text)

	}
}
