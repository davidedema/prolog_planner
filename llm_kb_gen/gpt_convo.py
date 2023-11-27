from asyncio.constants import SSL_HANDSHAKE_TIMEOUT
import os, sys
import argparse, yaml
from random import seed
from retry import retry
import openai
import re
from openai import OpenAI
from openai import AzureOpenAI

#=======================================================================#
#                           OPENAI API KEY                              #
#=======================================================================#
openai.api_key = os.environ["OPENAI_API_KEY"]                          #
#=======================================================================#


#===============================================#
# Available GPT versions:                       #
#=============================================================#
llm_versions = ["LLMPlanning","gpt-4","gpt-3.5-turbo"]        #
#=============================================================#


#===================================================================================================================#
#                                           INSERT TEST_CASE INTO A FILE                                            #
#===================================================================================================================#
def insert_test_case_into_file(file_path, test_case):
    try:
        with open(file_path, "w") as file:  # DO NOT APPEND # with open(file_path, "a") as file: # APPEND #
            # Inserting the content at the beginning of the file
            file.write(test_case)

        print("Content inserted successfully.")
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
    except Exception as e:
        print(f"An error occurred: {e}")



#===================================================================================================================#
#                                   EXTRACT A PROLOG CODE FROM LLM OUTPUT                                           #
#===================================================================================================================#
def extract_prolog_from_llm_output(multi_line_string):                                                           
    start_delimiter = "```"
    end_delimiter = "```"
    start_pos_prlg = multi_line_string.find(start_delimiter)
    end_pos_prlg = multi_line_string.find(end_delimiter, start_pos_prlg + len(start_delimiter))

    print("First code block start: ", start_pos_prlg, ", end_pos_domain: ", end_pos_prlg)

    if start_pos_prlg == -1 or end_pos_prlg == -1:
        return None
    return multi_line_string[start_pos_prlg + len(start_delimiter):end_pos_prlg].strip()



#===================================================================================================================#
#                                                   Send Query                                                      #
#===================================================================================================================#
@retry(tries=2, delay=30)
def connect_openai(engine, messages, temperature, max_tokens, top_p, frequency_penalty, presence_penalty, seed, stop):
    # UNITN-key
    client = AzureOpenAI(api_key=os.environ["OPENAI_API_KEY"], 
                         azure_endpoint="https://disi-logic-programming.openai.azure.com/", 
                         api_version='2023-05-15')
    response = client.chat.completions.create(
        model=engine,
        messages=messages,
        temperature= temperature,
        max_tokens=max_tokens,
        top_p=top_p,
        frequency_penalty=frequency_penalty,
        presence_penalty=presence_penalty,
        seed = seed,
        stop=stop
    )
    return response.choices[0].message.content


#===================================================================================================================#
#                                               GPT Class                                                           #
#===================================================================================================================#
class GPT_model:
    def __init__(self, engine, stop=None, max_tokens=2000, temperature=0.0, top_p=0, frequency_penalty=0.0, presence_penalty=0.0, seed=42):
        self.engine = engine
        self.max_tokens = max_tokens
        self.temperature = temperature              # 0
        self.top_p = top_p
        self.freq_penalty = frequency_penalty       # 0
        self.presence_penalty = presence_penalty    # 0
        self.stop = stop
        self.seed = seed                            # 42

    def get_response(self, prompt, messages=None, end_when_error=False, max_retry=5):
        conn_success, llm_output = False, ''
        if messages is not None:
            messages.append({'role': 'user', 'content': prompt})
        else:
            messages = [{'role': 'user', 'content': prompt}]
        n_retry = 0
        while not conn_success:
            n_retry += 1
            if n_retry >= max_retry:
                break
            try:
                print('[INFO] connecting to the LLM ...')
                llm_output = connect_openai(
                    engine=self.engine,
                    messages=messages,
                    temperature=self.temperature,
                    max_tokens=self.max_tokens,
                    top_p=self.top_p,
                    frequency_penalty=self.freq_penalty,
                    presence_penalty=self.presence_penalty,
                    seed = self.seed,
                    stop=self.stop
                )
                conn_success = True
            except Exception as e:
                print(f'[ERROR] LLM error: {e}')
                if end_when_error:
                    break
        return conn_success, llm_output



#===================================================================================================================#
#                                               	MAIN FUNCTION                                               #
#===================================================================================================================#
def main(llm_version, yaml_files):
    llm_gpt = GPT_model(engine=llm_version)
    messages = []
    system_msg = None

    if len(yaml_files)==0:
        print("No YAML file for training has been passed to the LLM. Continuiing with the default")
        # get directory of the script
        yaml_files.append(os.path.join(os.path.dirname(__file__), "few-shots.yaml"))


    # Open yaml file
    for file_name in yaml_files:
        print("Adding examples from file:", file_name)
        with open(file_name) as file:
            yaml_file = yaml.load(file, Loader=yaml.FullLoader)['few_shots']
            # Set headers for few-shots learning
            # Check if system_msg (a dict) has alrteady been added
            if not system_msg and 'system_msg' in yaml_file:
                system_msg = yaml_file['system_msg']
                messages.append(system_msg)
            elif system_msg and 'system_msg' in yaml_file:
                raise("System message had already been added")

            for msg_id in yaml_file['convo']:
                messages.append(yaml_file['convo'][msg_id]['Q'])
                messages.append(yaml_file['convo'][msg_id]['A'])            

    print(messages[-1])

    ## Here put the final query
    _, llm_output = llm_gpt.get_response("""                                         
        Can you provide a description of how an action should be code? 
        """,
        messages=messages,
        end_when_error=True
    )    

    # Print LLM output    
    print("=" * 82)
    print("=" * 35 + " LLM output " + "=" * 35)
    print("=" * 82)
    print(llm_output)
    print("=" * 82)
    print("\n" * 2)
    

    # Extract the prolog code from the llm output
    #prolog_code = extract_prolog_from_llm_output(llm_output)
    # Get current directory
    #current_dir = os.getcwd()
    # Directory of the prolog file 
    #prolog_file_path = current_dir + "/test_case.pl"
    
    #=============================================#
    #print("=" * 82)
    #print("=" * 32 + " PROLOG Test Case " + "=" * 32)
    #print("=" * 82)
    #print("prolog_file_path : " + prolog_file_path)
    #print("=" * 82)
    #print(prolog_code)    
    #print("=" * 82)
    #=============================================#
    #insert_test_case_into_file(prolog_file_path, prolog_code)



if __name__ == '__main__':

    # Initialize parser
    parser = argparse.ArgumentParser()
    
    # Adding optional argument
    parser.add_argument("-v", "--version", help = "ChatGPT version")
    parser.add_argument("-y", "--yaml_files", nargs='+', help = "A space defined list of YAML files containing the few-shots examples")
    
    # Read arguments from command line
    args = parser.parse_args()

    if args.version in llm_versions:
        llm_version = args.version
    else:
        if args.version == None:
            llm_version = llm_versions[0]
            print("The default version({}) has been selected".format(llm_version))
        else:
            print("Please choose an available version of GPT")
            print("Available versions : ")
            for v in llm_versions:
                print("\t - " + v)
            exit()
        if args.yaml_files != None:
            for file in args.yaml_files:
                assert(os.path.isfile(file)), "File {} does not exist".format(file)

    main(llm_version, args.yaml_files)
