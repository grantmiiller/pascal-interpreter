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

func IsFloat(s string) bool {
	_, err := strconv.ParseFloat(s, 64)
	if err == nil {
		return true
	}
	return false
}

func IsJustFloat(s string) bool {
	return IsFloat(s) && strings.Contains(s, ".")
}

// ToDigit is just a shortcut so I don't need to remember strconv
func ToDigit(s string) (int, error) {
	return strconv.Atoi(s)
}

// ToFloat is just shortcut so I don't need to remember strconv
func ToFloat(s string) (float64, error) {
	return strconv.ParseFloat(s, 64)
}

// float to string
func FToS(f float64) string {
	return fmt.Sprintf("%f", f)
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

// Token types
const (
	TAdd = "TAdd"
	TAssign = "TAssign"
	TBegin = "TBegin"
	TColon = "TColon"
	TComma = "TComma"
	TDot = "TDot"
	TEnd = "TEnd"
	TEoF = "TEoF"
	TFloatDiv = "TFloatDiv"
	TId = "TId"
	TInt = "TInt"
	TIntConst = "TIntConst"
	TIntDiv = "TIntDiv"
	TLParen = "TLParen"
	TMult = "TMult"
	TProgram = "TProgram"
	TRParen = "TRParen"
	TReal = "TReal"
	TRealConst = "TRealConst"
	TSemi = "TSemi"
	TSub = "TSub"
	TVar = "TVar"
)

var RESERVED_KEYWORDS = map[string]Token {
	"BEGIN": Token{Type: TBegin, Value: "BEGIN"},
	"DIV": Token{Type: TIntDiv, Value: "DIV"},
	"END": Token{Type: TEnd, Value: "End"},
	"INTEGER": Token{Type: TInt, Value: "TInt"},
	"PROGRAM": Token{Type: TProgram, Value: "PROGRAM"},
	"REAL": Token{Type: TReal, Value: "REAL"},
	"VAR": Token{Type: TVar, Value: "TVar"},
}

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

// Parses a string for all values that are numbers
func (l *Lexer) GetNumber() Token {
	val := ""	
	// Handles integers
	for ;IsInteger(l.Char); {
		val = val + l.Char
		l.Advance()
	}
	// We may be a float
	if l.Char == "." {
		val = val + l.Char
		l.Advance()

		for ;IsInteger(l.Char); {
			val = val + l.Char
			l.Advance()
		}
		return Token{Type:TRealConst, Value: val}
	}
	return Token{Type:TIntConst, Value: val}
}

func (l *Lexer) IgnoreWhiteSpace() {
	for ; IsSpace(l.Char); {
		l.Advance()
	}
}

func (l *Lexer) IgnoreComments() {
	if l.Char == "{" {
		l.Advance()
		for ; l.Char != "}"; {
			l.Advance()
		}
		l.Advance()
	}
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
	for ;!l.AtEOF(); {
		l.IgnoreWhiteSpace()

		if l.Char == "{" {
			l.IgnoreComments()
			continue
		}

		if l.AtEOF() {
			return Token{Type: TEoF, Value: ""}
		}

		c := l.Char

		if IsAlpha(l.Char) || c == "_" {
			return l.id()
		}

		if IsInteger(c) {
			return l.GetNumber() 
		}

		if c == ":" && l.Peek() == "=" {
			l.Advance()
			l.Advance()
			return Token{Type: TAssign, Value: ":="}
		}
		
		if c == ":" {
			l.Advance()
			return Token{Type: TColon, Value: ":"}
		}

		if c == "," {
			l.Advance()
			return Token{Type: TComma, Value: ","}
		}

		if c == "/" {
			l.Advance()
			return Token{Type: TFloatDiv, Value: "/"}
		}

		if c == ";" {
			l.Advance()
			return Token{Type: TSemi, Value: ";"}
		}
		if c == "." {
			l.Advance()
			return Token{Type: TDot, Value: "."}
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
	if l.AtEOF() {
		return Token{Type: TEoF, Value: ""}
	}
	panic("Should not ever reach here")
}

type Node struct {
	Type string
	Value string
	Token Token
	Children []*Node
	Meta map[string]string
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
	//NProgram
	NProgram = "NProgram"
	//NBlock
	NBlock = "NBlock"
	// NVarDecl
	NVarDecl = "NVarDecl" 
	// NType
	NType = "NType"
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

// block
// grammar: block -> declarations compoundStatement
func (p *Parser) block() *Node {
	children := p.declarations()
	// compound statement node
	n := p.compoundStatement()
	return &Node{Type: NBlock, Children: append(children,  n)}
}

// declarations
// grammar: declarations -> VAR (variableDeclaration TSemi) + | empty
func (p *Parser) declarations() []*Node {
	var nl []*Node
	if p.CurToken.Type == TVar {
		p.eat(TVar)
		for ;p.CurToken.Type == TId; {
			nl = append(nl, p.varDeclaration()...)
			p.eat(TSemi)
		}
	}
	return nl
}

// variableDeclaration
// grammar: variableDeclaration -> TId (TComma TId)* TColon typeSpec
func (p *Parser) varDeclaration() []*Node {
	// slice of variable nodes
	vl := []*Node{&Node{Type: NVar, Value: p.CurToken.Value, Token: p.CurToken}}
	p.eat(TId)

	for ;p.CurToken.Type == TComma; {
		p.eat(TComma)
		vl = append(vl, &Node{Type: NVar, Value: p.CurToken.Value, Token: p.CurToken})
		p.eat(TId)
	}

	p.eat(TColon)

	// The type of variables this group is
	tn := p.typeSpec()

	// slice of variable declaration nodes
	var nl []*Node

	for _, n := range vl {
		nl = append(nl, &Node{Type: NVarDecl, Children: []*Node{n, tn}})
	}

	return nl
}

// typeSpec
// grammar: typeSpec -> TInt | TReal
func (p *Parser) typeSpec() *Node {
	t := p.CurToken
	switch t.Type {
	case TInt: 
		p.eat(TInt)
	case TReal:
		p.eat(TReal)
	default:
		fmt.Printf("Found: %s", t.ToString())
		panic("Panic due to unexpected variable type")
	}
	return &Node{Type: NType, Value: t.Type, Token: t}
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
// grammar: program -> TProgram variable TSemi block TDot
func (p *Parser) program() *Node {
	p.eat(TProgram)
	n := p.variable()
	pName := n.Value
	p.eat(TSemi)

	blockNode := p.block()
	programNode := Node{Type: NProgram, Value: pName, Children: []*Node{blockNode}}

	p.eat(TDot)

	return &programNode 
}

// factor
// grammar: factor -> TAdd factor
//									| TSub factor
//									| TInt
//									| TLParen Expr TRParen
//									| variable
func (p *Parser) Factor() *Node {
	t := p.CurToken
	
	// If it is a TAdd or TSub, we return a unary op node
	if ContainsString(t.Type, []string{TAdd, TSub}) {
		p.eat(t.Type)
		return &Node{Type: NUnOp, Value: t.Value, Token: t, Children: []*Node{
				p.Factor(),
			},
		}
	}

	if t.Type == TIntConst {
		p.eat(TIntConst)
		return &Node{Type: NNum, Value: t.Value, Token: t}
	}

	if t.Type == TRealConst {
		p.eat(TRealConst)
		return &Node{Type: NNum, Value: t.Value, Token: t}
	}

	// This is a parentheses expression, ie (1 + 2)
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

	for ;ContainsString(p.CurToken.Type, []string{TMult, TIntDiv, TFloatDiv}); {
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

func (i *Interpreter) visitVarDecl(n Node) string {
	return ""
}

func (i *Interpreter) visitType (n Node) string {
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
	leftS := i.visit(*n.Children[0])
	rightS := i.visit(*n.Children[1])

	// We need to know whether these values are floats or ints
	// We default to floats since we are less likely to lose data
	isFloat := false

	if IsJustFloat(leftS) || IsJustFloat(rightS) {
		isFloat = true
	}

	left, err := ToFloat(i.visit(*n.Children[0]))
	if err != nil {
		panic("Left value was not a number")	
	}

	right, err := ToFloat(i.visit(*n.Children[1]))
	if err != nil {
		panic("Right value was not a number")	
	}
	
	switch n.Token.Type {
	case TMult:
		if isFloat {
			return FToS(left * right)
		}
		return strconv.Itoa(int(left) * int(right))
	case TIntDiv:
		return strconv.Itoa(int(left) / int(right))
	case TFloatDiv:
		return FToS(left / right)
	case TAdd:
		if isFloat {
			return FToS(left + right)
		}
		return strconv.Itoa(int(left) + int(right))
	case TSub:
		if isFloat {
			return FToS(left - right)
		}
		return strconv.Itoa(int(left) - int(right))
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


func (i *Interpreter) visitBlock(n Node) {
	for _, c := range n.Children[:len(n.Children)-1] {
		i.visit(*c)
	}
	i.visit(*n.Children[len(n.Children)-1])
}

func (i *Interpreter) visitProgram(n Node) {
	i.visit(*n.Children[0])
}

func (i *Interpreter) visit(n Node) string {
	if n.Type == NoOp {
		return i.visitNoOp(n)
	}

	if n.Type == NVarDecl{
		return i.visitNoOp(n)
	}

	if n.Type == NType {
		return i.visitNoOp(n)
	}

	if n.Type == NProgram {
		i.visitProgram(n)
		return ""
	}

	if n.Type == NBlock {
		i.visitBlock(n)
		return ""
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

	return i.visit(*tree)
}

func printSubTree(nl []*Node) {
	if len(nl) > 0 {
		var children []*Node 

		for _, c := range nl {
			fmt.Printf("<T: %s, V: %s, C: %d>    ", c.Type, c.Value, len(c.Children))
			children = append(children, c.Children...)
		}
		
		fmt.Printf("\n")

		printSubTree(children)
	}
}

func printTree(n Node) {
	fmt.Printf("<T: %s, V: %s, C: %d>\n", n.Type, n.Value, len(n.Children))

	printSubTree(n.Children)
}

func main() {
	l := NewLexer(`
		PROGRAM Part10AST;
		VAR
			 a, b : INTEGER;
			 y    : REAL;

		BEGIN {Part10AST}
			 a := 2;
			 b := 10 * a + 10 * a DIV 4;
			 y := 20 / 7 + 3.14;
		END.  {Part10AST}
	`)
	p := NewParser(l)
	i := NewInterpreter(p)
	i.Interpret()
	i.PrintGScope()
}
