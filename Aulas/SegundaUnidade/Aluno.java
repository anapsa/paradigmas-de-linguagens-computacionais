public class Aluno {
    private Curso curso; 
    private String nome; 
    private int idade; 
    private String cpf; 

    public Aluno(Curso curso, String nome, int idade, String cpf){
        this.curso = curso;
        this.nome = nome;
        this.idade = idade; 
        this.cpf = cpf; 
    }
    public void setCurso(Curso curso) {
        this.curso = curso;
    }
    public Curso getCurso() {
        return this.curso;
    }
    public String getNome() {
        return this.nome;
    }
    public int getIdade() {
        return this.idade;
    }
    public String cpf() {
        return this.cpf;
    }
    public void printDados() {
        System.out.println(nome); 
        System.out.println(idade); 
        System.out.println(cpf); 
        this.curso.printCurso(); 
    }
}
