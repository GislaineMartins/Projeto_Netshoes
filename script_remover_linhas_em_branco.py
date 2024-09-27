#Script para remover as linhas em branco

def remove_blank_lines(input_file, output_file):
    try:
        # Abre o arquivo de entrada para leitura com codificação 'utf-8'
        with open(input_file, 'r', encoding='utf-8') as infile:
            # Lê todas as linhas do arquivo
            lines = infile.readlines()
        
        # Filtra as linhas, removendo as linhas em branco (que possuem apenas espaços ou estão vazias)
        non_blank_lines = [line for line in lines if line.strip()]
        
        # Abre o arquivo de saída para escrita com codificação 'utf-8'
        with open(output_file, 'w', encoding='utf-8') as outfile:
            outfile.writelines(non_blank_lines)
        
        print(f'Linhas em branco removidas e resultado salvo em {output_file}')
    
    except UnicodeDecodeError as e:
        print(f"Erro de decodificação: {e}")
        print("Tente usar uma codificação diferente ou verifique o conteúdo do arquivo.")

# Nome do arquivo de entrada e saída
input_file = 'valor.txt'   # Substitua pelo nome do seu arquivo de entrada
output_file = 'valor_limpo.txt' # Nome do arquivo onde as linhas sem branco serão salvas

# Chama a função para remover as linhas em branco
remove_blank_lines(input_file, output_file)
