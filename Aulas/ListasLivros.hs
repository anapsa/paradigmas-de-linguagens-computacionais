type Pessoa = String
type Livro = String 
type BancoDados = [(Pessoa,Livro)]

--livros emprestados
baseExemplo :: BancoDados 
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando", "Duna")] 

--fazer uma versão usando compressão de listas 
livros1 :: BancoDados -> Pessoa -> Livro
livros1 bd pess = [liv | (p,liv) <- bd, p == pess] 

--fazer uma versão usando filter 
livros2 :: BancoDados -> Pessoa -> Livro
livros2 bd pess = map snd (filter procurapessoa bd) --map snd pega o segundo elemento da tupla
  where procurapessoa (p,1) = (p == pess) 

--usando filter e função lambda
livros3 :: BancoDados -> Pessoa -> Livro
livros3 bd pess = map snd (filter (\(p,1) -> (p == pess)) bd) 

--compreensão de listas
emprestimos1, emprestimos2 :: BancoDados -> Livro -> [Pessoa] 
emprestimos1 bd livr = [ pess | (p,1) <- bd, l == liv] 
--filter
emprestimos2 bd livr = map fst (filter (\(p,1) -> l == livr) bd)

emprestado :: BancoDados -> Livro -> Bool 
emprestado bd livro = (emprestimos1 bd livro /= []) --reutilizar códigos já feitos 

qtdEmprestimos :: BancoDados -> Pessoa -> Int 
qtdEmprestimos bd pessoa = length (livros1 bd pess) --pega o comprimento da lista que tem retorno de todos os livros de uma pessoa 

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados 
emprestar bd pess livro = (pess,livro) : bd 

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados 
--devolver bd pess livro = filter(\(p1,l1) -> (p1,l1) /= (pess,livro)) bd 
