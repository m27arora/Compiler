#include <iostream>
#include <vector>
#include<memory>
#include <map>
#include <sstream>
using namespace std;


struct Node {
	Node(string v) : rule(v) {}
	string rule;
	vector<Node*> children;
};

string printcode(const Node* t, map<string, int>& location, int& i, string& name, map<string, map<string, string>>& tables);
string push(string str);

string firstWord (string str) {
	return str.substr(0, str.find(" "));
}


int RHS (string str){

	int words = 0; // Holds number of words

	for(int i = 0; str[i] != '\0'; i++)
	{
		if (str[i] == ' ') //Checking for spaces
		{
			words++;
		}
	}
	return words;
}



int numChild (string rule){  

	string F = firstWord(rule);

	if( F=="BOF" || F=="BECOMES" || F=="COMMA" || F=="ELSE" || F=="EOF" || F=="EQ" || F=="GE" || F=="GT" || F=="ID" || F=="IF" || F=="INT" || F=="LBRACE" ||F=="LE"||F=="LPAREN"|| F=="LT" || F=="MINUS"||F=="NE"||F=="NUM"||F=="PCT"|| F=="PLUS"||F=="PRINTLN"|| F=="RBRACE"||F=="RETURN"||F=="RPAREN"||F=="SEMI"||F=="SLASH"||F=="STAR"||F=="WAIN"||F=="WHILE"||F=="AMP"||F=="LBRACK"||F=="RBRACK"||F=="NEW"||F=="DELETE"||F=="NULL" ) {
		return 0;}

	int countRHS = RHS(rule);

	return countRHS;

}

Node *createTree() {
	int child;
	string line;

	getline(std::cin, line);

	Node *node = new Node(line);
	child = numChild(line);
	for (int i = 0; i<child; ++i){
		node->children.push_back(createTree());
	}
	return node;
}


void deleteTree(Node* node)  
{  
	if (node == NULL) return;

	for (int i = 0; i<node->children.size(); ++i){
		deleteTree(node->children[i]);
	}

	delete node;
}

void create(const Node *t, map<string, string>& table, map<string, int>& location, int& i){

	if(t->rule == "dcl type ID"){
		i-=4;
		string lexeme = t->children[1]->rule.substr(3);
		string type;
		if(t->children[0]->children.size()==2){
			type = "int*";}
		else{
			type = "int";}
		if(table.find(lexeme) != table.end()){
			std::cerr<<"ERROR DUPLICATE FOUND"<<std::endl;
			return;
		}
		table.insert({lexeme, type});
		location.insert({lexeme,i });

	}

	else{
		for ( Node* child : t -> children ){
			create(child, table, location, i);
		}
	}

}

void findmain(const Node* t, map<string, int>& location, int& i, string& name, map<string, map<string, string>>& tables){
	if(t->rule =="main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		if(t->children[9]->children.size()!=0){
			cout<<printcode(t->children[9], location, i, name, tables);
		}
		if(t->children[8]->children.size()!=0){
			cout<<printcode(t->children[8], location, i, name, tables);
		}
		cout<<printcode(t->children[11], location, i, name, tables);

	}
	else{
		for(Node* child : t->children){
			findmain(child, location, i, name, tables);}
	}
}

void traverse_param(const Node *t, string& params){

	if (t->children[0]->children[0]->children.size()==2) {
		params.append("int* ");
	}
	else {
		params.append("int ");
	}
	if (t->children.size()!=1) {
		traverse_param(t->children[2], params);
	}
}

void parameters(const Node *t, string& params){

	if(t->rule=="params paramlist" || t->rule =="procedures main"){

		if(t->rule =="procedures main"){

			if (t->children[0]->children[3]->children[0]->children.size() == 2)
				params.append("int* ");
			else
				params.append("int ");


			if (t->children[0]->children[5]->children[0]->children.size() == 2)
				params.append("int *");
			else
				params.append("int");

		}
		else{

			traverse_param(t->children[0], params);

		}
	}

	else{
		for(Node * child : t->children){
			parameters(child, params);
		}
	}

}

bool declareBeforeUse(const Node *t, map<string, string>& table,bool& found){

	if(t->rule == "factor ID" || t->rule =="lvalue ID"){
		string var = t->children[0]->rule.substr(3);

		if ( table.find(var) == table.end() )
			found = false;
	}
	else{
		for( Node* child : t->children ){
			declareBeforeUse(child, table, found);
		}
	}

	return found;
}

string checksignature(string &id, string& procname, map<string, map<string, string>>& table){

	string args = "";

	for(auto const& pair: table){
		if(pair.first==procname){
			for(auto const& p: pair.second){
				if(p.first == procname){
					args = p.second;}
			}
		}
	}

	return args;
}


string checktype(const Node *t, string& ltype, string& rtype, string& procname, map<string, map<string, string>>& tables){

	if(t->rule == "expr expr PLUS term" ){
		string l = checktype(t->children[0], ltype, rtype, procname, tables);
		string r = checktype(t->children[2], ltype, rtype, procname, tables);
		if (l =="int" && r =="int"){
			return "int";
		}
		if((l =="int*" && r =="int") ||(l =="int" && r =="int*")){
			return "int*";
		}
		return " plus ERROR MISMATCH";
	}

	if(t->rule == "expr expr MINUS term"){
		string l = checktype(t->children[0], ltype, rtype, procname, tables);
		string r = checktype(t->children[2], ltype, rtype, procname, tables);
		if(l=="int" && r=="int"){
			return "int";
		}
		if(l=="int*"&& r=="int"){
			return "int*";
		}
		if(l=="int*" && r=="int*"){
			return "int";
		}

		return " ERROR MISMATCH MINUS";
	}

	if(t->children.size()==1){
		return checktype(t->children[0], ltype, rtype, procname, tables);
	}

	if(t->children.size()==0){

		if(firstWord(t->rule)=="NUM"){
			return "int";}

		else if(firstWord (t->rule) == "NULL"){
			return "int*";}

		string varname = t->rule.substr(3);

		for(auto const& pair: tables){
			if(pair.first == procname){
				for(auto const& p: pair.second){
					if(varname == p.first){
						return p.second;
					}
				}
			}
		}
	}

	if(t->rule == "factor STAR factor"|| t->rule =="lvalue STAR factor"){
		string ctype = checktype(t->children[1], ltype, rtype, procname, tables);
		if(ctype != "int*"){
			return " ERROR CTYPE ";
		}
		return "int";
	}

	if(t->rule == "factor AMP lvalue"){
		string ctype = checktype(t->children[1], ltype, rtype, procname, tables);
		if(ctype != "int"){
			return "ERROR AMP";}
		return "int*";}

	if(t->rule == "factor NEW INT LBRACK expr RBRACK"){
		string ctype = checktype (t->children[3], ltype, rtype, procname, tables);
		if(ctype!="int"){
			return "ERROR";
		}
		return "int*";}

	if(t->rule == "factor LPAREN expr RPAREN"){
		return checktype(t->children[1], ltype, rtype, procname, tables);
	}

	if(t->rule == "lvalue LPAREN lvalue RPAREN"){
		return checktype(t->children[1], ltype, rtype, procname, tables);
	}

	if(t->rule == "factor ID LPAREN RPAREN"){
		string id = t->children[0]->rule.substr(3);
		string str = checksignature(id, procname, tables);
		if(str!=""){
			return "ERROR";
		}
		return "int";
	}

	if(t->rule == "factor ID LPAREN arglist RPAREN"){
		string id = t->children[0]->rule.substr(3);
		string str = checksignature(id, procname, tables);
		string args = checktype(t->children[2], ltype, rtype, procname, tables);

		if(str!=args){
			return "ERROR ARGSLIST DOES NOT MATCH";
		}
		return "int";

	}

	if(t->rule =="arglist expr" || t->rule =="arglist expr COMMA arglist"){
		if(t->children.size()==1){
			return checktype(t->children[0], ltype, rtype, procname, tables);
		}
		else{
			string expr = checktype(t->children[0], ltype, rtype, procname, tables);
			string arglist = checktype(t->children[2], ltype, rtype, procname, tables);
			string ans = "";
			ans.append(expr);
			ans.append(" ");
			ans.append(arglist);
			return ans;
		}
	}


	if( t->rule == "term term STAR factor" || t->rule =="term term SLASH factor" || t->rule=="term term PCT factor"){
		string term = checktype(t->children[0], ltype, rtype, procname, tables);
		string factor = checktype(t->children[2], ltype, rtype, procname, tables);
		if(!(term =="int" && factor =="int")){
			return "ERROR";}
		return "int";
	}

	if(firstWord(t->rule)=="test"){
		string l = checktype (t->children[0], ltype, rtype, procname, tables);
		string r = checktype (t->children[2], ltype, rtype, procname, tables);

		if(l!=r){
			return "ERROR TEST STMT";}
	}

	else{return "";}   }


	string computeType(const Node *t, string& procname, map<string, map<string, string>>& tables) {


		if(t->rule=="lvalue ID"){
			string varname = t->children[0]->rule.substr(3);

			for(auto const& pair: tables){
				if(pair.first == procname){
					for(auto const &p: pair.second){
						if(varname == p.first){
							string ltype = p.second;
							return ltype;
						}
					}
				}
			}

		}
		else{
			string ltype="";
			string rtype="";
			return checktype(t, ltype, rtype, procname,  tables);
		}
		return "";
	}

void typeCheck(const Node *t, string& procedurename, map<string, map<string, string>>& tables){

	if ( t-> rule == "statement lvalue BECOMES expr SEMI"){

		string ltype = computeType(t->children[0], procedurename, tables);
		string rtype = computeType(t->children[2], procedurename, tables);
		if(ltype != rtype){
			std::cerr<<"ERROR TYPE MISMATCH"<<std::endl;
		}
	}

	if(t->rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){

		string type = computeType(t->children[2], procedurename, tables);
		if(type!="int"){
			std::cerr<<"ERROR PRINTLN"<<std::endl;
		}
	}

	if(t->rule == "statement DELETE LBRACK RBRACK expr SEMI"){

		string type = computeType(t->children[3], procedurename, tables);

		if(type!="int*"){

			std::cerr<<"ERROR DELETE"<<std::endl;
		}
	}

	if(t->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		string exprtype = computeType(t->children[11], procedurename, tables);
		if(exprtype!="int"){
			std::cerr<<"ERROR AFTER MAIN EXPR";
		}
	}

	if(t->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		string expr = computeType(t->children[9], procedurename, tables);
		if(expr!="int"){
			std::cerr<<"ERROR AFTER PROCEDURE EXPR";
		}
	}

	if (t->rule == "dcls dcls dcl BECOMES NULL SEMI"){

		string type = t->children[1]->children[0]->rule;

		if(type!= "type INT STAR"){
			std::cerr<<"ERROR DCLS INT STAR"<<std::endl;}
	}

	if( t->rule == "dcls dcls dcl BECOMES NUM SEMI"){

		string type = t->children[1]->children[0]->rule;

		if(type!="type INT"){
			std::cerr<<"ERROR DCLS INT"<<std::endl;
		}
	}

	if(firstWord(t->rule)=="test"){
		string l = computeType (t->children[0], procedurename, tables);
		string r = computeType (t->children[2], procedurename, tables);

		if(l!=r){
			std::cerr<<"ERROR TEST STMT"<<std::endl
				;}
	}



	else{

		for( Node * child: t->children ){
			typeCheck(child, procedurename, tables);
		}
	}
}


bool procedurecalls(const Node* t, map<string, string>& ptable){

	if(t->rule =="factor ID LPAREN RPAREN"|| t->rule == "factor ID LPAREN arglist RPAREN"){

		bool exist = false;

		string funcname = t->children[0]->rule.substr(3);

		for(auto const& pair :ptable){
			if(pair.first == funcname){
				exist = true;
			}}

		if(exist == false){
			std::cerr<<"ERROR PROC NOT DECLARED"<<std::endl;
			return false;
		}
		else{return true;}

	}

	else{

		for( Node * child: t->children){
			procedurecalls(child, ptable);
		}
	}
}

void createTable(const Node *t, map<string, string>& symtable, map<string, map<string, string>>& table, map<string,string>& ptable, map<string, int>& location ){

	if(t->rule == "procedures main"|| t->rule.substr(0,16)=="procedure INT ID"){
		int i=4;
		symtable.clear();
		create(t, symtable,location,  i);
		string parameter="";
		parameters(t, parameter) ;
		string wain = "wain";


		bool d = true;
		bool declare = declareBeforeUse(t, symtable, d);

		if(!declare){
			std::cerr<<"ERROR NOT DECALRED AND USED "<<std::endl;
			return;
		}



		if(t->rule == "procedures main"){
			table.insert({"wain",symtable});
			ptable.insert({"wain", parameter});
			typeCheck(t, wain, table);

			if(t->children[0]->children[5]->children[0]->rule != "type INT"){
				std::cerr<<"ERROR AFTER MAIN DCL";
				return;
			}

		}
		else{
			string str = t->children[1]->rule.substr(3);
			if(ptable.find(str)!=ptable.end()){
				std::cerr<<"ERROR PROCEDURE REPEATED"<<std::endl;
				return;}
			table.insert({str, symtable});
			ptable.insert({str, parameter});
			typeCheck(t, str, table);

		}

		bool proc = procedurecalls(t, ptable);

	}
	else{
		for(Node * child: t->children){
			createTable(child,symtable, table, ptable, location);}
	}
}

int loadlocation(const Node* t, map<string, map<string, string>>& table, map<string, int>& location, int& loc){

	if(t->rule == "expr term"){
		string id;
		id = t->children[0]->children[0]->children[0]->rule.substr(3);

		int i=1;


		i=location.find(id)->second;


		if(i==1){
			loc= 4;}
		else{loc= 0;}
	}

	else{
		for( Node* child: t->children){
			loadlocation(child, table, location, loc);
		}
	}

	return loc;
}

void printasm (const Node* t,map<string,  map<string, string>>& table, map<string, int>& location){

	std::cout<<"; begin prologue"<<endl;
	cout<<"lis $4 ; new convention $4 always contains 4"<<endl;
	cout<<".word 4"<<endl;
	cout<<"sw $1 , -4( $30 ) ; store variable a at $30 -4"<<endl;
	cout<<"sw $2 , -8( $30 ) ; store variable b at $30 -8"<<endl;
	cout<<"sub $30 , $30 , $4"<<endl;
	cout<<"sub $30 , $30 , $4"<<endl;
	cout<<"; end prologue"<<endl;
	int loc =0;
	loadlocation(t, table, location, loc);
	cout<<"lw $3 , "<<loc<<"( $30 ) ; load from stack location reserved for variable a"<<endl;
	cout<<"; begin epilogue"<<endl;
	cout<<"add $30 , $30 , $4"<<endl;
	cout<<"add $30 , $30 , $4"<<endl;
	cout<<"jr $31"<<endl;
}

void printvariables(const Node* t, int& count){
	if(t->rule.substr(0,3)=="NUM"){
		count++;
		string  s = t->rule.substr(4);
		std::stringstream word(s);
		int number = 0;
		word >> number;

		cout<<"lis $5 \n.word "<<number<<endl;
		cout<<push("$5")<<endl;
	}
	else{
		for(Node* child: t->children){
			printvariables(child, count);
		}
	}
}

string push(string str){
	string s =  "sw ";
	s.append(str);
	s.append(" , -4( $30 ) \nsub $30 ,$30 ,$4\n");
	return s;
}


string pop(string str){
	string s = "\nadd $30 ,$30 ,$4 \nlw ";
	s.append(str);
	s.append(" , -4( $30 )\n");
	return s;
}


string code(string str, map<string, int>& location){

	string offset = to_string(location.find(str)->second);
	string s = "\nlw $3 , ";
	s.append(offset);
	s.append("( $29 )\n");
	return s;
}

string printcode(const Node* t, map<string, int>& location, int& i, string& name, map<string ,map<string, string>>& tables){

	if(t->rule == "factor NUM"){
		string str= "lis $3 \n.word ";
		str.append(t->children[0]->rule.substr(4));
		return str;
	}

	else if(t->rule.substr(0,2)=="ID"){
		string str=code(t->rule.substr(3), location);

		return str;
	}
	else if( t->rule == "factor LPAREN expr RPAREN"){
		return printcode(t->children[1], location, i, name, tables);
	}

	else if( t->children.size()==1){
		return printcode(t->children[0], location, i, name, tables);
	}
	else if(t->rule == "expr expr PLUS term"){
		string expr = computeType(t->children[0], name, tables);
		string term = computeType(t->children[2], name, tables);

		if( expr == "int*" && term == "int"){

			string str = printcode(t->children[0], location, i, name, tables);
			str = str + push("$3") + printcode(t->children[2], location, i, name, tables);
			str = str + "\nmult $3, $4 \nmflo $3 \n" + pop("$5") + "\nadd $3, $5, $3\n";

			return str;

		}

		else if( expr =="int" && term=="int*"){

			string str = printcode(t->children[2], location, i, name, tables);
			str = str + push("$3") + printcode(t->children[0], location, i, name, tables);
			str = str + "\nmult $3, $4 \nmflo $3 \n" + pop("$5") + "\nadd $3, $5, $3\n";

			return str;


		}


		else{
			string str="";
			str.append(printcode(t->children[0], location, i, name, tables));
			str.append("\n");
			str.append(push("$3"));
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));
			str.append("\nadd $3 , $5 , $3\n");
			return str;
		}
	}

	else if(t->rule == "expr expr MINUS term"){

		string expr = computeType(t->children[0], name, tables);
		string term = computeType(t->children[2], name, tables);

		if( expr == "int*" && term == "int"){

			string str = printcode(t->children[0], location, i, name, tables);
			str = str + push("$3") + printcode(t->children[2], location, i, name, tables);
			str = str + "\nmult $3, $4 \nmflo $3 \n" + pop("$5") + "\nsub $3, $5, $3\n";

			return str;

		}

		else if( expr =="int" && term=="int*"){

			string str = printcode(t->children[2], location, i, name, tables);
			str = str + push("$3") + printcode(t->children[0], location, i, name, tables);
			str = str + "\nmult $3, $4 \nmflo $3 \n" + pop("$5") + "\nsub $3, $5, $3\n";

			return str;


		}

		else if( expr =="int*" && term =="int*"){

			string str = printcode(t->children[0], location, i, name, tables);
			str = str + push("$3");
			str = str + printcode(t->children[2], location, i, name, tables);
			str = str + pop("$5") + "\nsub $3, $5, $3 \ndiv $3, $4 \nmflo $3\n";
			return str;
		}

		else{
			string str="";
			str.append(printcode(t->children[0], location, i, name, tables));
			str.append("\n");
			str.append(push("$3"));
			str.append(printcode(t->children[2], location,i , name, tables));
			str.append("\n");
			str.append(pop("$5"));
			str.append("\nsub $3 , $5 , $3\n");
			return str;
		}
	}

	else if(t->rule == "term term STAR factor"){
		string str="";
		str.append(printcode(t->children[0], location, i, name, tables));
		str.append("\n");
		str.append(push("$3"));
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\n");
		str.append(pop("$5"));
		str.append("\nmult $5, $3 \nmflo $3\n");
		return str;
	}
	else if(t->rule == "term term SLASH factor"){         
		string str="";
		str.append(printcode(t->children[0], location, i, name, tables));
		str.append("\n");
		str.append(push("$3"));
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\n");
		str.append(pop("$5"));
		str.append("\ndiv $5 , $3 \nmflo $3");
		return str;
	}
	else if(t->rule =="term term PCT factor"){
		string str="";
		str.append(printcode(t->children[0], location, i, name, tables));
		str.append("\n");
		str.append(push("$3"));
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\n");
		str.append(pop("$5"));
		str.append("\ndiv $5 , $3 \nmfhi $3");
		return str;

	}

	else if(t->rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
		string str = (push("$1"));
		str.append("\n");
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\n");
		str.append( "add $1 , $3 , $0\n");
		str.append(push("$31"));
		str.append("\nlis $5 \n.word print");
		str.append("\njalr $5\n");
		str.append(pop("$31"));
		str.append(pop("$1"));
		return str;
	}


	else if(t->rule == "statements statements statement"){
		string str = printcode(t->children[0], location, i, name, tables);
		str.append( printcode(t->children[1], location, i, name, tables));
		return str;
	}

	else if (t->rule == "dcls dcls dcl BECOMES NUM SEMI"){
		string str = "";
		return str;
	}

	else if(t->rule == "lvalue ID"){

		return (t->children[0]->rule.substr(3));
	}

	else if(t->rule == "lvalue LPAREN lvalue RPAREN"){
		return printcode(t->children[1], location, i, name, tables);
	}

	else if(t->rule == "test expr LT expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){

			string str = printcode(t->children[0], location,i , name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location,i , name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $3, $5, $3\n");
			return str;
		}
		else
		{
			string str = printcode(t->children[0], location,i , name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location,i , name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $3, $5, $3\n");
			return str;
		}


	}
	else if(t->rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
		string loop = "loop";
		loop.append(to_string(i));
		string end = "end";
		end.append(to_string(i));
		i++;
		string str = loop;
		str.append(":\n");
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\nbeq $3, $0, ");
		str.append(end);
		str.append("\n");
		str.append(printcode(t->children[5], location, i, name, tables));

		str.append("\nbeq $0 ,$0, ");
		str.append(loop);
		str.append("\n");
		str.append(end);
		str.append(":\n");
		return str;

	}

	else if(t->rule =="test expr EQ expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){

			string str = printcode(t->children[0], location,i , name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $6, $3, $5 \nsltu $7, $5, $3 \nadd $3, $6, $7 \nsub $3, $11, $3\n ");
			return str;
		}
		else
		{

			string str = printcode(t->children[0], location,i , name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $6, $3, $5 \nslt $7, $5, $3 \nadd $3, $6, $7 \nsub $3, $11, $3\n ");
			return str;
		}
	}


	else if(t->rule == "test expr NE expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $6, $3, $5 \nsltu $7,$5,$3 \nadd $3, $6, $7\n");
			return str;
		}
		else
		{
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $6, $3, $5 \nslt $7,$5,$3 \nadd $3, $6, $7\n");
			return str;
		}
	}

	else if(t->rule == "test expr LE expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $3, $3, $5 \nsub $3, $11, $3\n");
			return str;

		}
		else
		{
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $3, $3, $5 \nsub $3, $11, $3\n");
			return str;
		}


	}
	else if(t->rule == "test expr GE expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $3, $5, $3 \nsub $3, $11, $3\n");
			return str;
		}
		else
		{
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $3, $5, $3 \nsub $3, $11, $3\n");
			return str;
		}


	}
	else if(t->rule == "test expr GT expr"){

		string type = computeType(t->children[0], name, tables);

		if(type == "int*"){
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nsltu $3, $3, $5\n");
			return str;
		}
		else {
			string str = printcode(t->children[0], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append("\n");
			str.append(printcode(t->children[2], location, i, name, tables));
			str.append("\n");
			str.append(pop("$5"));

			str.append("\nslt $3, $3, $5\n");
			return str;
		}

	}


	else if(t->rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){
		string elsee = "else";
		elsee.append(to_string(i));
		string end = "end";
		end.append(to_string(i));
		i++;
		string str = "";
		str.append(printcode(t->children[2], location, i, name, tables));
		str.append("\nbeq $3, $0, ");
		str.append(elsee);
		str.append("\n");
		str.append(printcode(t->children[5], location, i, name, tables));

		str.append("\nbeq $0 ,$0, ");
		str.append(end);
		str.append("\n");
		str.append(elsee);
		str.append(":\n");
		str.append(printcode(t->children[9], location, i, name, tables));
		str.append("\n");
		str.append(end);
		str.append(":\n");
		return str;

	}



	else if(t->rule == "factor NULL"){
		string str = "\nadd $3, $0, $11\n";
		return str;
	}

	else if(t->rule == "factor STAR factor"){
		string str = printcode(t->children[1], location, i, name, tables);
		str.append("\nlw $3, 0($3)\n");
		return str;

	}


	else if( t->rule == "factor AMP lvalue"){
		if(t->children[1]->rule == "lvalue ID"){
			string id = t->children[1]->children[0]->rule.substr(3);
			string offset = to_string(location.find(id)->second);
			string str = "\nlis $3 \n.word ";
			str.append(offset);
			str.append("\nadd $3, $3, $29\n");

			return str;

		}
		else if(t->children[1]->rule == "lvalue STAR factor"){
			return printcode(t->children[1]->children[1], location, i, name, tables);
		}
	}

	else if(t->rule == "statement lvalue BECOMES expr SEMI"){
		if(t->children[0]->rule == "lvalue ID"){
			string str = printcode(t->children[2], location, i, name, tables);
			string id = t->children[0]->children[0]->rule.substr(3);
			string offset = to_string(location.find(id)->second);
			str.append("\nsw $3, ");
			str.append(offset);
			str.append("($29)\n");
			return str;

		}
		else if(t->children[0]->rule == "lvalue STAR factor"){
			string str = printcode(t->children[2], location, i, name, tables);
			str.append("\n");
			str.append(push("$3"));
			str.append(printcode(t->children[0]->children[1], location, i, name, tables));
			str.append(pop("$5"));
			str.append("\nsw $5, 0($3)\n");
			return str;
		}
	}

	else if( t->rule == "factor NEW INT LBRACK expr RBRACK"){
		string str = printcode(t->children[3], location, i, name, tables);
		str.append("\nadd $1, $3, $0\n");
		str.append(push("$31"));
		str.append("\nlis $5 \n.word new \njalr $5 \n");
		str.append(pop("$31"));
		str.append("\nbne $3, $0, 1 \nadd $3, $11, $0\n");
		return str;
	}


	else if( t->rule == "statement DELETE LBRACK RBRACK expr SEMI")
	{ string skip = "skip";
		skip.append(to_string(i));
		i++;
		string str = printcode(t->children[3], location, i, name, tables);
		str = str + "\nbeq $3, $11, " + skip + "\nadd $1, $3, $0\n";
		str = str + push("$31") + "\nlis $5 \n.word delete \njalr $5 \n";
		str = str + pop("$31") + skip + ":\n";

		return str;



	}


	else{return "";}

}


void print(const Node* t, map<string, int>& location, bool& parameter, string procname, map<string, map<string, string>>& tables){
	cout<< "; begin prologue \n";
	cout<<".import print \n.import init \n.import new \n.import delete \nlis $4 \n.word 4 \nlis $10 \n.word print \nlis $11 \n.word 1 \nsub $29 , $30 , $4 \n";
	cout<<push("$1");
	cout<<push("$2")<<endl;
	int count=0;
	printvariables(t, count);
	int i =0;

	cout<<"; calling init"<<endl;
	cout<<push("$2")<<endl;
	cout<<push("$31")<<endl;
	if(!parameter){
		cout<<"add $2, $0, $0"<<endl;
	}
	cout<<"lis $5 \n.word init \n jalr $5"<<endl;
	cout<<pop("$31")<<endl;
	cout<<pop("$2")<<endl;
	cout<<"; end prologue;"<<endl<<endl;

	findmain(t, location, i, procname, tables);

	cout<<endl<<";begin epilogue"<<endl;
	cout<<"add $30 , $30 , $4"<<endl;
	cout<<"add $30 , $30 , $4"<<endl;
	for(int i=0;i<count;i++){
		cout<<"add $30 , $30 , $4"<<endl;
	}
	cout<<"jr $31"<<endl;
}


int main() {
	Node *root = createTree();

	map<string, string> symbolTable;
	map<string, map<string, string>> tables;
	map<string, string> ptable;
	map<string, int> location;

	createTable(root, symbolTable, tables, ptable, location);

	string wain = "wain";
	string param = ptable.find(wain)->second;
	string wainparam = param.substr(0, 4);
	bool firstparam = false;
	if (wainparam == "int*"){
		firstparam= true;
	}

	print(root, location, firstparam, "wain", tables);

	deleteTree(root);
	return 0;
}

