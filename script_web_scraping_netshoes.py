import time
from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from bs4 import BeautifulSoup
import pandas as pd

# Set up the Chrome WebDriver
driver = webdriver.Chrome()

# Define the base URL and search keyword
search_keyword = "tenis%20masculino"
base_url = "https://www.netshoes.com.br/busca?nsCat=Natural&q="

# Define the number of pages to scrape
total_pages = 1

nomes = []
valores = []

# Loop through the pages
for page in range(1, total_pages + 1):
    # Construct the URL with the page number
    url = f"{base_url}{search_keyword}&page={page}"
    
    # Navigate to the page
    driver.get(url)

    # Set scroll height and speed
    scroll_pause_time = 0.8  # Adjust this value to control the speed of scrolling
    scroll_increment = 200 # Pixels to scroll in each step

    # Get the initial scroll height
    last_height = driver.execute_script("return document.body.scrollHeight")

    while True:
        # Scroll down by the specified increment
        driver.execute_script(f"window.scrollBy(0, {scroll_increment});")
        time.sleep(scroll_pause_time)

        # Calculate the new scroll height after scrolling
        new_height = driver.execute_script("return window.pageYOffset + window.innerHeight")

        # Break the loop if you've reached the bottom of the page
        if new_height >= last_height:
            break

    # Get the page source after scrolling
    page_source = driver.page_source

    # Parse the page source with BeautifulSoup
    soup = BeautifulSoup(page_source, "html.parser")

    # Extract and print search results
  
    cont = 0
    search_results = soup.find_all("div", class_="card__description")
    for result in search_results:
        nome = result.find('h2', class_='card__description--name')
        if nome:
            nome = nome.text.strip()
            
        else:
            nome = 'Nome não encontrado'
            
        print(f'{nome}')
        nomes.append(nome)

        valor = result.find('span', class_='full-mounted')
                
        if valor:
            valor = valor.text.strip()
            
        else:
            valor = 'R$0,00'            
            
        print(f'{valor}')
        valores.append(valor)
        
        cont += 1

    print(f"Total de produtos encontrados na página {page}: {cont}")

    
    

# Create the DataFrame
data = pd.DataFrame(
    list(zip(nomes, valores)),
    columns=['Nomes', 'Valor']
)

# Remove rows where 'Valor' contains the word "Pix"
data['Valor'] = data['Valor'].str.replace("no Pix", "", regex=False)
# Remove rows where 'Valor' contains the aspas
data['Valor'] = data['Valor'].str.replace(" '' ", "", regex=False)

# Export data to a CSV file
data.to_csv("C:\\Users\\gimcj\\Desktop\\netshoes\\dados\\netshoes_dados.csv", index=False, encoding='utf-8-sig')
    
# Fechar o navegador ao terminar
driver.quit()
