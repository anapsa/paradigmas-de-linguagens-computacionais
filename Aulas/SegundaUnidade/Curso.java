public class Curso {
    private int cod;
    private String nome; 

    public Curso(int cod, String nome) {
        this.cod = cod;
        this.nome = nome;
    }
    public void setNome (String nome) {
        this.nome = nome;
    }
    public void setCod(int cod) {
        this.cod = cod; 
    }
    public int getCodigo () {
        return this.cod;
    }
    public void printCurso() {
        System.out.println(this.cod); 
        System.out.println(this.nome);
    }
}
