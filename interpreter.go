package main

import (
	"fmt"
	"strings"
	"strconv"
	"regexp"
	"unicode/utf8"
)

func IsAlphaNum(s string) bool {
	re := regexp.MustCompile(`[a-zA-Z0-9]`)
	return re.MatchString(s)	
}

func IsAlpha(s string) bool {
	re := regexp.MustCompile(`[a-zA-Z]`)
	return re.MatchString(s)	
}

func IsSpace(s string) bool {
	re := regexp.MustCompile(`\s`)
	return re.MatchString(s)
}

// IsInteger returns true is string can be an integer
func IsInteger(s string) bool {
	_, err := strconv.Atoi(s)
	if err == nil {
		return true
	}
	return false
}

// ToDigit is just a shortcut so I don't need to remember strconv
func ToDigit(s string) (int, error) {
	return strconv.Atoi(s)
}

// ContainsString returns true if string is in list
func ContainsString(a string, list []string) bool {
	for _, b := range list {
		if b == a {
			return true
		}
	}
	return false
}

func StringLen(text string) int {
	return len([]rune(text))
}

type Token struct {
	Type string
	Value string
}

func (t Token) ToString() string {
	return fmt.Sprintf("Token: <Type: %s, Value: %s", t.Type, t.Value)
}

const (
	TInt = "TInt"
	TEoF = "TEoF"
	TMult = "TMult"
	TAdd = "TAdd"
	TSub = "TSub"
	TDiv = "TDiv"
	TLParen = "TLParen"
	TRParen = "TRParen"
	TBegin = "TBegin"
	TEnd = "TEnd"
	TDot = "TDot"
	TAssign = "TAssign"
	TSemi = "TSemi"
	TId = "TId"
)

type Lexer struct {
	Text string
	Pos int
	Char string
}

func NewLexer(text string) Lexer {
	if text == "" {
		panic("Must not be an empty string")
	}
	return Lexer{
		Text: text,
		Pos: 0,
		Char: string([]rune(text)[0]),
	}
}

func (l Lexer) AtEOF() bool {
	return l.Pos >= StringLen(l.Text)
}

func (l *Lexer) CharAt(i int) string {
	return string([]rune(l.Text)[i]);
}

// Parses a string for all values that are integers
func (l *Lexer) GetInteger() string {
	val := ""	
	for ;IsInteger(l.Char); {
		val = val + l.Char
		l.Advance()
	}
	return val
}

func (l *Lexer) IgnoreWhiteSpace() {
	for ; IsSpace(l.Char); {
		l.Advance()
	}
}

var RESERVED_KEYWORDS = map[string]Token {
	"BEGIN": Token{Type: TBegin, Value: "BEGIN"},
	"END": Token{Type: TEnd, Value: "End"},
	"DIV": Token{Type: TDiv, Value: "DIV"},
}

func (l *Lexer) id() Token{
	result := l.Char
	l.Advance()

	for ;l.Char != "" && IsAlphaNum(l.Char); {
		result = result + l.Char
		l.Advance()
	}

	if val, ok := RESERVED_KEYWORDS[strings.ToUpper(result)]; ok {
		return val
	}
	return Token{Type: TId, Value: strings.ToUpper(result)}
}

func (l *Lexer) Peek() string {
	peekPos := l.Pos + 1
	if peekPos > StringLen(l.Text) {
		return ""
	}	
	return l.CharAt(peekPos)
}

func (l *Lexer) Advance() {
	l.Pos++
	if !l.AtEOF() {
		l.Char = l.CharAt(l.Pos)
	} else {
		l.Char = ""
	}
}

func (l *Lexer) GetNextToken() Token {
	l.IgnoreWhiteSpace()

	if l.AtEOF() {
		return Token{Type: TEoF, Value: ""}
	}

	c := l.Char

	if IsAlpha(l.Char) || c == "_" {
		return l.id()
	}

	if c == ":" && l.Peek() == "=" {
		l.Advance()
		l.Advance()
		return Token{Type: TAssign, Value: ":="}
	}

	if c == ";" {
		l.Advance()
		return Token{Type: TSemi, Value: ";"}
	}

	if c == "." {
		l.Advance()
		return Token{Type: TDot, Value: "."}
	}

	if IsInteger(c) {
		return Token{Type: TInt, Value: l.GetInteger()}
	}
	if c == "+" {
		l.Advance()
		return Token{Type: TAdd, Value: c}
	}
	if c == "-" {
		l.Advance()
		return Token{Type: TSub, Value: c}
	}
	if c == "*" {
		l.Advance()
		return Token{Type: TMult, Value: c}
	}
	if c == "(" {
		l.Advance()
		return Token{Type: TLParen, Value: c}
	}
	if c == ")" {
		l.Advance()
		return Token{Type: TRParen, Value: c}
	}

	fmt.Printf("Failed to parse char: %s\n", c)
	r, size := utf8.DecodeRuneInString(c)
	fmt.Printf("Unicode Info: %d %v\n", r, size)
	panic("Do not understand character");
}

type Node struct {
	Type string
	Value string
	Token Token
	Children []*Node
}

const (
	// Number type node
	NNum = "NNum"
	// Binary Operator type node
	NBinOp = "NBinOp"
	// Unary Operator type node
	NUnOp = "NUnOp"
	// Compound statement type node
	NComp = "NComp"
	// Variable assignment type node
	NAssign = "NAssign"
	// Variable representation type node
	NVar = "NVar"
	// NoOp
	NoOp = "NoOp"
)

func (n Node) ToString() string {
	return fmt.Sprintf("Node: <Type: %s, Value: %s>", n.Type, n.Value)
}

type Parser struct {
	l Lexer
	CurToken Token
}

func NewParser(l Lexer) Parser {
	return Parser{
		l: l,
		CurToken: l.GetNextToken(),
	}
}

func (p *Parser) eat(t string) {
	if p.CurToken.Type == t {
		p.CurToken = p.l.GetNextToken()
	} else {
		fmt.Printf("Unexpected token found: %s\n", p.CurToken.ToString())
		panic("Panicking");
	}
}

// noop
// Just an empty statement
func (p *Parser) noop() *Node {
	return &Node{Type: NoOp, Value: ""}
}

// variable
// grammar: variable -> TId
func (p *Parser) variable() *Node {
	n := Node{Type: NVar, Value: p.CurToken.Value, Token: p.CurToken}
	p.eat(TId)
	return &n
}

// assignmentStatement
// grammar: assignmentStatement -> variable TAssign expr
func (p *Parser) assignmentStatement() *Node {
	left := p.variable()
	token := p.CurToken
	p.eat(TAssign)
	right := p.Expr()
	return &Node{Type: NAssign, Value: token.Value, Token: token, Children: []*Node{
		left, right,
	}}
}

// statement
// grammar: statement -> compoundStatement | assignmentStatement | NoOp 
func (p *Parser) statement() *Node {
	var n *Node
	if p.CurToken.Type == TBegin {
		n = p.compoundStatement()
	} else if p.CurToken.Type == TId {
		n = p.assignmentStatement()
	} else {
		n = p.noop()
	}
	return n
}

// statementList
// grammar statementList -> statement | statement TSemi statementList
func (p *Parser) statementList() []*Node {
	node := p.statement()	
	nodes := []*Node{node}

	for ;p.CurToken.Type == TSemi; {
		p.eat(TSemi)
		nodes = append(nodes, p.statement())
	}

	if p.CurToken.Type == TId {
		panic("We got an unexpected ID")
	}
	
	return nodes
}

// compoundStatement
// grammar: compoundStatement -> TBegin statementList TEnd
func (p *Parser) compoundStatement() *Node {
	p.eat(TBegin)
	nodes := p.statementList()
	p.eat(TEnd)
	return &Node{Type: NComp, Value: "", Children: nodes}
}

// program
// grammar: program -> compoundStatement TDot
func (p *Parser) program() *Node {
	node := p.compoundStatement()	
	p.eat(TDot)
	return node
}

// factor
// grammar: factor -> TAdd factor
//									| TSub factor
//									| TInt
//									| TLParen Expr TRParen
//									| variable
func (p *Parser) Factor() *Node {
	t := p.CurToken
	if ContainsString(t.Type, []string{TAdd, TSub}) {
		p.eat(t.Type)
		return &Node{Type: NUnOp, Value: t.Value, Token: t, Children: []*Node{
				p.Factor(),
			},
		}
	}
	if t.Type == TInt {
		p.eat(TInt)
		return &Node{Type: NNum, Token: t, Value: t.Value}
	}
	if t.Type == TLParen {
		p.eat(TLParen)
		node := p.Expr()
		p.eat(TRParen)
		return node
	}
	if t.Type == TId {
		return p.variable()
	}
	fmt.Printf("Erroring in Factor with token: %s\n", p.CurToken.ToString())
	panic("Could not parse token")
}

func (p *Parser) Term() *Node {
	node := p.Factor()	
	for ;ContainsString(p.CurToken.Type, []string{TMult, TDiv}); {
		t := p.CurToken
		p.eat(t.Type)
		node = &Node{Type: NBinOp, Value: t.Value, Token: t, Children: []*Node{
				node,
				p.Factor(),
			},
		}
	}
	return node
}

func (p *Parser) Expr() *Node {
	node := p.Term()	
	for ;ContainsString(p.CurToken.Type, []string{TAdd, TSub}); {
		t := p.CurToken
		p.eat(t.Type)
		node = &Node{Type: NBinOp, Value: t.Value, Token: t, Children: []*Node{
				node,
				p.Term(),
			},
		}
	}

	return node
}

func (p *Parser) Parse() *Node {
	n := p.program()
	if p.CurToken.Type != TEoF{
		panic("Something bad happened")
	}
	return n
}

const (
	SNum = "SNum"
	SString = "SString"
)

type Interpreter struct {
	p Parser
	// Global Scope "Symbol Table"
	// Will probably need to contain a value and a type
	GScope map[string]string
}

func NewInterpreter(p Parser) Interpreter {
	return Interpreter{
		p: p,
		GScope: make(map[string]string),
	}
}

func (i *Interpreter) visitNoOp(n Node) string {
	return ""
}

func (i *Interpreter) visitCompound(n Node) {
	for _, c := range n.Children {
		i.visit(*c)
	}
}

func (i *Interpreter) visitVar(n Node) string {
	name := n.Value
	val, ok := i.GScope[name]
	if !ok {
		fmt.Printf("Undefined Variable: %s", name)
		panic("Freaking out")
	}
	return val
}

func (i *Interpreter) visitAssign(n Node) {
	name := n.Children[0].Value
	r := i.visit(*n.Children[1])
	i.GScope[name] = r
}

func (i *Interpreter) visitBinOp(n Node) string {
	left, err := ToDigit(i.visit(*n.Children[0]))
	if err != nil {
		panic("Left value was not a number")	
	}
	right, err := ToDigit(i.visit(*n.Children[1]))
	if err != nil {
		panic("Right value was not a number")	
	}
	switch n.Token.Type {
	case TMult:
		return strconv.Itoa(left * right)
	case TDiv:
		return strconv.Itoa(left / right)
	case TAdd:
		return strconv.Itoa(left + right)
	case TSub:
		return strconv.Itoa(left - right)
	}
	fmt.Printf("Don't understand this binop: %s\n", n.ToString())
	panic("Unknown Binop")
}

func (i *Interpreter) visitUnOp(n Node) string {
	oper, err := ToDigit(i.visit(*n.Children[0]))
	if err != nil {
		panic("Operand value was not a number")	
	}

	switch n.Token.Type {
	case TAdd:
		return strconv.Itoa(+oper)
	case TSub:
		return strconv.Itoa(-oper)
	}
	fmt.Printf("Failed on Node: %s\n", n.ToString())
	panic("Cant perform unary operator with node")
}

func (i *Interpreter) visit(n Node) string {
	if n.Type == NoOp {
		return i.visitNoOp(n)
	}

	if n.Type == NComp {
		i.visitCompound(n)
		return ""
	}

	if n.Type == NAssign {
		i.visitAssign(n)
		return ""
	}

	if n.Type == NVar {
		return i.visitVar(n)
	}

	if n.Type == NNum {
		return n.Value
	}

	if n.Type == NUnOp{
		return i.visitUnOp(n)
	}

	if n.Type == NBinOp {
		return i.visitBinOp(n)
	}

	fmt.Printf("Failed on Node: %s\n", n.ToString())
	panic("Cant perform binary operator with node")
}

func (i *Interpreter) PrintGScope() {
	for k, v := range i.GScope { 
		fmt.Printf("<k:[%s], v:[%s]\n", k, v)
	}
}

func (i *Interpreter) Interpret() string {
	tree := i.p.Parse()

	// For Debugging
	fmt.Println("PRINTING TREE")
	fmt.Println("===============")
	printTree(*tree)
	fmt.Printf("\n\n")

	return i.visit(*tree)
}

// Prints a node tree in reverse because lazy
func printTree(n Node) {
	for _, c := range n.Children {
		printTree(*c)
	}
	if len(n.Children) != 0 {
		fmt.Printf("\n")
	}
	fmt.Printf("<T: %s, V: %s>", n.Type, n.Value)
}

func main() {
	l := NewLexer(`
		BEGIN

				BegIN
						_nUMber := 2;
						a := _Number;
						b := 10 * a + 10 * _number div 4;
						c := a - - b
				END;

				x := 11;
		END.
	`)
	p := NewParser(l)
	i := NewInterpreter(p)
	i.Interpret()
	i.PrintGScope()
}
