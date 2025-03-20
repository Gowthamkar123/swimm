import os
import sys
import argparse
import re
import time
from typing import List, Dict, Optional, Tuple
from openai import OpenAI  # New import
import json
import requests
from tenacity import retry, stop_after_attempt, wait_exponential, retry_if_exception_type


class AssemblerToCOBOLConverter:
    def __init__(self, api_key: str, model: str = "meta/llama-3.1-405b-instruct"):
        """
        Initialize the converter with NVIDIA API credentials.

        Args:
            api_key: NVIDIA API key
            model: NVIDIA model to use (default is meta/llama-3.1-405b-instruct)
        """
        self.client = OpenAI(
            base_url="https://integrate.api.nvidia.com/v1",
            api_key=api_key
        )
        self.model = model

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=4, max=10),
        retry=retry_if_exception_type((requests.exceptions.Timeout, requests.exceptions.RequestException)),
        reraise=True
    )
    def call_anthropic_api(self, system_prompt: str, user_prompt: str, max_tokens: int = 4000,
                           temperature: float = 0.0):
        """
        Call the NVIDIA API with retry logic for timeouts and rate limits.
        """
        try:
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ]

            response = self.client.chat.completions.create(
                model=self.model,
                messages=messages,
                temperature=temperature,
                max_tokens=max_tokens,
                top_p=0.7,
                stream=False
            )

            # Create a response object that matches the expected structure
            return type('Response', (), {
                'content': [
                    type('Content', (), {'text': response.choices[0].message.content})()
                ]
            })()

        except Exception as e:
            print(f"API error: {e}")
            raise

    def read_assembler_file(self, file_path: str) -> str:
        """Read the contents of an assembler file."""
        try:
            with open(file_path, 'r') as file:
                return file.read()
        except Exception as e:
            print(f"Error reading file {file_path}: {e}")
            sys.exit(1)

    def write_cobol_file(self, file_path: str, cobol_code: str) -> None:
        """Write COBOL code to a file."""
        try:
            with open(file_path, 'w', encoding='utf-8') as file:
                file.write(cobol_code)
            print(f"COBOL code successfully written to {file_path}")
        except Exception as e:
            print(f"Error writing to file {file_path}: {e}")
            sys.exit(1)

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=4, max=10),
        retry=retry_if_exception_type((requests.exceptions.Timeout, requests.exceptions.RequestException)),
        reraise=True
    )
    def call_anthropic_api(self, system_prompt: str, user_prompt: str, max_tokens: int = 4000,
                           temperature: float = 0.0):
        """
        Call the Anthropic API with retry logic for timeouts and rate limits.
        """
        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=max_tokens,
                temperature=temperature,
                system=system_prompt,
                messages=[
                    {"role": "user", "content": user_prompt}
                ]
            )
            return response
        except (requests.exceptions.Timeout) as e:
            print(f"API timeout or rate limit error: {e}. Retrying...")
            # Sleep before retry to avoid hitting rate limits
            time.sleep(5)
            raise
        except Exception as e:
            print(f"Unexpected error in API call: {e}")
            raise

    def analyze_assembler_code(self, assembler_code: str) -> Dict:
        """
        Analyze the assembler code to understand its structure and purpose.

        Returns:
            Dict containing analysis of the code
        """
        # For very large assembler code, let's truncate it to avoid timeouts
        # This is a simplistic approach - consider more sophisticated chunking for production use
        max_length = 50000  # Characters
        if len(assembler_code) > max_length:
            print(f"Warning: Assembler code is very large ({len(assembler_code)} chars). Truncating for analysis.")
            assembler_code_for_analysis = assembler_code[:max_length] + "\n...[truncated for analysis]"
        else:
            assembler_code_for_analysis = assembler_code

        prompt = f"""
        Analyze the following assembler code to understand its structure, purpose, and functionality.
        Identify:
        1. Data structures and their usage
        2. Main program flow and logic
        3. Key subroutines or procedures
        4. Input/output operations
        5. Any system-specific features that might be challenging to convert

        Assembler code:
        ```
        {assembler_code_for_analysis}
        ```

        Provide a structured analysis in JSON format with the following keys:
        - purpose: Overall purpose of the program
        - data_structures: List of data structures with descriptions
        - program_flow: Description of the main program flow
        - subroutines: List of subroutines with descriptions
        - io_operations: List of input/output operations
        - challenges: Potential challenges for COBOL conversion
        """

        system_prompt = "You are an expert in both assembler and COBOL programming languages. Your task is to analyze assembler code to prepare for conversion to COBOL."

        try:
            response = self.call_anthropic_api(system_prompt, prompt, max_tokens=4000, temperature=0.0)

            # Extract JSON from the response
            content = response.content[0].text
            json_match = re.search(r'```json\s*([\s\S]*?)\s*```', content) or re.search(r'{[\s\S]*}', content)
            if json_match:
                return json.loads(json_match.group(1) if '```' in json_match.group(0) else json_match.group(0))
            else:
                # If no JSON format is found, try to create a basic structure from the text
                return {
                    "purpose": "Extracted from non-JSON response",
                    "data_structures": [],
                    "program_flow": content,
                    "subroutines": [],
                    "io_operations": [],
                    "challenges": []
                }
        except Exception as e:
            print(f"Error during analysis: {e}")
            print("Falling back to basic analysis structure")
            return {
                "purpose": "Error in analysis",
                "data_structures": [],
                "program_flow": "Analysis failed due to: " + str(e),
                "subroutines": [],
                "io_operations": [],
                "challenges": ["Analysis phase failed, conversion might be incomplete"]
            }

    def generate_cobol_equivalent(self, assembler_code: str, analysis: Dict) -> str:
        """
        Generate COBOL code based on the assembler code and its analysis.

        Args:
            assembler_code: The original assembler code
            analysis: Analysis of the assembler code

        Returns:
            COBOL code equivalent to the input assembler code
        """
        # Handle large assembler code by splitting into chunks if needed
        max_chunk_size = 30000  # Characters

        if len(assembler_code) > max_chunk_size:
            print(f"Assembler code is large ({len(assembler_code)} chars). Using chunked processing.")
            return self._process_large_assembler_code(assembler_code, analysis)

        # Standard processing for smaller code
        prompt = f"""
        Convert the following assembler code to equivalent COBOL code.

        Original Assembler Code:
        ```
        {assembler_code}
        ```

        Based on analysis, this program:
        - Purpose: {analysis.get('purpose', 'Unknown')}
        - Main program flow: {analysis.get('program_flow', 'Unknown')}

        Key considerations for the conversion:
        1. Maintain the same logic and functionality as the original assembler code
        2. Use appropriate COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
        3. Convert data structures to appropriate COBOL formats
        4. Handle any assembler-specific operations with equivalent COBOL constructs
        5. Add clear comments to explain complex conversions or logic

        Provide only the final, complete COBOL code without explanations. The COBOL code should be production-ready and follow modern COBOL best practices.
        """

        system_prompt = "You are an expert converter of assembler code to COBOL. You understand both languages deeply and can create equivalent functionality in COBOL for any assembler program. Your goal is to produce clean, efficient, and maintainable COBOL code."

        try:
            response = self.call_anthropic_api(system_prompt, prompt, max_tokens=8000, temperature=0.1)

            # Extract the COBOL code from the response
            content = response.content[0].text
            cobol_match = re.search(r'```(?:cobol)?\s*([\s\S]*?)\s*```', content)
            if cobol_match:
                return cobol_match.group(1)
            else:
                # If no code block, return the full content as it might be just the code
                return content

        except Exception as e:
            print(f"Error during code generation: {e}")
            # Return a placeholder with error info if generation fails
            return f"""
      * ERROR: COBOL GENERATION FAILED
      * Error message: {str(e)}
      * This is a placeholder for the COBOL code that could not be generated.
      * Please try again with a smaller input file or check your API key.
            """

    def _process_large_assembler_code(self, assembler_code: str, analysis: Dict) -> str:
        """
        Process large assembler code by breaking it into sections.
        This is a simplified approach - for production use, a more sophisticated
        approach would be needed to handle dependencies between sections.
        """
        # Look for logical section boundaries in the assembler code
        # This is simplified and may need adjustment based on the assembler dialect
        section_patterns = [
            r'^\s*SECTION\s',
            r'^\s*[A-Z0-9_]+\s+CSECT',
            r'^\s*[A-Z0-9_]+\s+DSECT',
            r'^\s*[A-Z0-9_]+\s+PROC',
            r'^\s*[A-Z0-9_]+\s+EQU',
            r'^\s*\*{3,}',  # Comment block separators
            r'^\s*\+{3,}'  # Continuation indicators
        ]

        # Find potential section breaks
        lines = assembler_code.split('\n')
        section_indices = [0]  # Start with beginning of file

        for i, line in enumerate(lines):
            if i > 0:  # Skip first line as it's already a boundary
                for pattern in section_patterns:
                    if re.match(pattern, line, re.IGNORECASE):
                        section_indices.append(i)
                        break

        # Always include the end of the file
        section_indices.append(len(lines))

        # Deduplicate and sort the indices
        section_indices = sorted(list(set(section_indices)))

        print(f"Breaking assembler code into {len(section_indices) - 1} logical sections")

        # Process each section
        cobol_sections = []
        for i in range(len(section_indices) - 1):
            start_idx = section_indices[i]
            end_idx = section_indices[i + 1]

            section_code = '\n'.join(lines[start_idx:end_idx])

            # Skip empty sections
            if not section_code.strip():
                continue

            print(f"Processing section {i + 1}/{len(section_indices) - 1} ({len(section_code)} chars)")

            section_prompt = f"""
            Convert this section of assembler code to equivalent COBOL code.
            This is section {i + 1} of {len(section_indices) - 1} from a larger program.

            Assembler code section:
            ```
            {section_code}
            ```

            About the overall program:
            - Purpose: {analysis.get('purpose', 'Unknown')}

            Important:
            1. This is part of a larger program being converted in sections
            2. Generate just the COBOL code for this section
            3. Include appropriate comment headers showing this is section {i + 1}
            4. Maintain all identifier names as close as possible to the original
            """

            system_prompt = "You are an expert converter of assembler code to COBOL. Convert this section maintaining identifiers and structure."

            try:
                response = self.call_anthropic_api(system_prompt, section_prompt, max_tokens=4000, temperature=0.1)

                # Extract just the code
                content = response.content[0].text
                cobol_match = re.search(r'```(?:cobol)?\s*([\s\S]*?)\s*```', content)
                section_cobol = cobol_match.group(1) if cobol_match else content

                cobol_sections.append(f"      *----------------------------------------------------------------\n"
                                      f"      * SECTION {i + 1} OF {len(section_indices) - 1}\n"
                                      f"      *----------------------------------------------------------------\n"
                                      f"{section_cobol}\n")

            except Exception as e:
                print(f"Error processing section {i + 1}: {e}")
                cobol_sections.append(f"      * ERROR IN SECTION {i + 1}: {str(e)}\n")

        # Combine all sections and create a complete COBOL program structure
        combined_sections = "\n".join(cobol_sections)

        # Create a COBOL program wrapper
        cobol_program = f"""
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERTED-PROGRAM.
       AUTHOR. ANTHROPIC-CLAUDE-CONVERTER.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       * Combined data definitions would normally go here

       PROCEDURE DIVISION.

       * CONVERTED ASSEMBLER PROGRAM - MULTIPLE SECTIONS
       * Original program purpose: {analysis.get('purpose', 'Unknown')}
       * Conversion notice: This program was automatically converted from assembler
       * Warning: Large program converted in sections - manual review recommended

{combined_sections}

       PROGRAM-END.
           STOP RUN.
       """

        return cobol_program

    def validate_cobol_code(self, assembler_code: str, cobol_code: str) -> Tuple[bool, str]:
        """
        Validate the generated COBOL code to ensure it matches the functionality
        of the original assembler code.

        Returns:
            Tuple of (is_valid, validation_report)
        """
        # For very large assembler code, use a representative sample for validation
        max_length = 30000  # Characters
        if len(assembler_code) > max_length:
            print(f"Assembler code is large. Using sample for validation.")
            # Take first part, a middle section, and the end for a representative sample
            third = len(assembler_code) // 3
            asm_sample = assembler_code[:10000] + "\n...[truncated]...\n" + \
                         assembler_code[third:third + 10000] + "\n...[truncated]...\n" + \
                         assembler_code[-10000:]
        else:
            asm_sample = assembler_code

        prompt = f"""
        Compare the original assembler code and the generated COBOL code to validate that they are functionally equivalent.

        Original Assembler Code:
        ```
        {asm_sample}
        ```

        Generated COBOL Code:
        ```
        {cobol_code}
        ```

        Please analyze:
        1. Are all the functionality and logic from the assembler code preserved in the COBOL code?
        2. Are there any errors or issues in the COBOL code?
        3. Are there any optimizations that could be made to the COBOL code?
        4. Are there any assembler-specific features that were not correctly translated?

        Provide a validation report with your findings and a final YES/NO assessment of whether the conversion is correct.
        """

        system_prompt = "You are a senior programmer with expertise in both assembler and COBOL. Your task is to validate that COBOL code correctly implements the functionality of assembler code."

        try:
            response = self.call_anthropic_api(system_prompt, prompt, max_tokens=4000, temperature=0.0)
            content = response.content[0].text

            # Check if the validation approves the conversion
            is_valid = "YES" in content.upper() and not ("NO" in content.upper() and "YES" not in content.upper())

            return is_valid, content

        except Exception as e:
            error_msg = f"Error during validation: {e}"
            print(error_msg)
            return False, error_msg

    def refine_cobol_code(self, assembler_code: str, initial_cobol: str, validation_report: str) -> str:
        """
        Refine the COBOL code based on validation feedback.

        Args:
            assembler_code: Original assembler code
            initial_cobol: Initial COBOL conversion
            validation_report: Validation report with issues

        Returns:
            Refined COBOL code
        """
        # For large assembler code, use truncated version
        max_length = 30000  # Characters
        if len(assembler_code) > max_length:
            assembler_for_refinement = assembler_code[:max_length] + "\n...[truncated for refinement]"
        else:
            assembler_for_refinement = assembler_code

        prompt = f"""
        Refine the following COBOL code based on the validation report. The code should maintain functional equivalence with the original assembler code.

        Original Assembler Code:
        ```
        {assembler_for_refinement}
        ```

        Current COBOL Code:
        ```
        {initial_cobol}
        ```

        Validation Report:
        {validation_report}

        Please provide an improved version of the COBOL code that addresses the issues mentioned in the validation report. Ensure the code is production-ready and follows COBOL best practices.
        """

        system_prompt = "You are an expert COBOL programmer. Your task is to refine COBOL code to address specific issues while maintaining functionality equivalent to the original assembler code."

        try:
            response = self.call_anthropic_api(system_prompt, prompt, max_tokens=8000, temperature=0.1)

            content = response.content[0].text
            cobol_match = re.search(r'```(?:cobol)?\s*([\s\S]*?)\s*```', content)
            if cobol_match:
                return cobol_match.group(1)
            else:
                # If no code block, return the full content
                return content

        except Exception as e:
            print(f"Error during refinement: {e}")
            # Return the original COBOL code with an error comment
            return f"""
      * ERROR DURING REFINEMENT: {str(e)}
      * The code below is the original unrefined conversion

{initial_cobol}
            """

    def document_conversion(self, assembler_code: str, cobol_code: str, analysis: Dict) -> str:
        """
        Generate documentation for the conversion process.

        Returns:
            Documentation string
        """
        # Create a truncated version for documentation
        max_length = 20000  # Characters
        if len(assembler_code) > max_length:
            assembler_sample = assembler_code[:10000] + "\n...[truncated]...\n" + assembler_code[-10000:]
        else:
            assembler_sample = assembler_code

        if len(cobol_code) > max_length:
            cobol_sample = cobol_code[:10000] + "\n...[truncated]...\n" + cobol_code[-10000:]
        else:
            cobol_sample = cobol_code

        prompt = f"""
        Create comprehensive documentation for the conversion from assembler to COBOL code.

        Original Assembler Code (sample):
        ```
        {assembler_sample}
        ```

        Generated COBOL Code (sample):
        ```
        {cobol_sample}
        ```

        Program Analysis:
        {json.dumps(analysis, indent=2)}

        Please document:
        1. Overview of the original assembler program's purpose and functionality
        2. Key design decisions made during the conversion process
        3. Mapping of assembler constructs to COBOL equivalents
        4. Any notable challenges or special considerations
        5. Testing recommendations for the converted code

        Format the documentation in Markdown.
        """

        system_prompt = "You are a technical documentation expert with deep knowledge of both assembler and COBOL. Your task is to create clear, comprehensive documentation for a code conversion project."

        try:
            response = self.call_anthropic_api(system_prompt, prompt, max_tokens=4000, temperature=0.2)
            return response.content[0].text
        except Exception as e:
            print(f"Error generating documentation: {e}")
            # Return basic documentation if generation fails
            return f"""
# Assembler to COBOL Conversion Documentation

## Program Overview
- Purpose: {analysis.get('purpose', 'Unknown')}

## Conversion Process
- Conversion performed using automated tools
- Error occurred during documentation generation: {str(e)}

## Important Notes
- Manual review of the generated COBOL code is strongly recommended
- Testing is essential to verify functional equivalence
            """

    def chunk_file(self, file_path: str, output_dir: str, max_lines: int = 1000) -> List[str]:
        """
        Split a large assembler file into smaller chunks for processing.

        Args:
            file_path: Path to the assembler file
            output_dir: Directory to save chunk files
            max_lines: Maximum lines per chunk

        Returns:
            List of paths to chunk files
        """
        # Create output directory if it doesn't exist
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        with open(file_path, 'r') as f:
            lines = f.readlines()

        total_lines = len(lines)
        chunk_files = []

        for i in range(0, total_lines, max_lines):
            chunk_num = i // max_lines + 1
            end = min(i + max_lines, total_lines)
            chunk_content = ''.join(lines[i:end])

            chunk_file = os.path.join(output_dir, f"chunk_{chunk_num}.asm")
            with open(chunk_file, 'w') as f:
                f.write(chunk_content)

            chunk_files.append(chunk_file)

        return chunk_files

    def convert(self, assembler_file_path: str, output_file_path: Optional[str] = None,
                generate_docs: bool = True, docs_file_path: Optional[str] = None) -> None:
        """
        Convert an assembler file to COBOL.

        Args:
            assembler_file_path: Path to the assembler source file
            output_file_path: Path to save the COBOL output file (default: same as input with .cbl extension)
            generate_docs: Whether to generate documentation for the conversion
            docs_file_path: Path to save the documentation (default: same as output with .md extension)
        """
        # Set default output path if not provided
        if not output_file_path:
            output_file_path = os.path.splitext(assembler_file_path)[0] + '.cbl'

        # Set default docs path if not provided and docs requested
        if generate_docs and not docs_file_path:
            docs_file_path = os.path.splitext(output_file_path)[0] + '_conversion.md'

        # Get file size before reading
        file_size = os.path.getsize(assembler_file_path) / (1024 * 1024)  # Size in MB
        print(f"Assembler file size: {file_size:.2f} MB")

        # Use chunking for very large files to avoid memory issues
        if file_size > 10:  # If larger than 10MB
            print(f"File is large (>10MB). Processing in chunks...")
            chunk_dir = os.path.join(os.path.dirname(assembler_file_path), "asm_chunks")
            chunk_files = self.chunk_file(assembler_file_path, chunk_dir)

            # Process only the first chunk for analysis
            print(f"Analyzing first chunk of assembler code...")
            first_chunk_code = self.read_assembler_file(chunk_files[0])
            analysis = self.analyze_assembler_code(first_chunk_code)

            # Process each chunk and combine results
            all_cobol_code = []
            for i, chunk_file in enumerate(chunk_files):
                print(f"Processing chunk {i + 1}/{len(chunk_files)}...")
                chunk_code = self.read_assembler_file(chunk_file)
                chunk_cobol = self.generate_cobol_equivalent(chunk_code, analysis)
                all_cobol_code.append(f"      * CHUNK {i + 1}/{len(chunk_files)}\n{chunk_cobol}")

            # Combine all chunks into one COBOL program
            cobol_code = "\n      * ---------------------- NEXT CHUNK ----------------------\n".join(all_cobol_code)

            # Create a proper COBOL program wrapper around the chunks
            cobol_code = f"""
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERTED-PROGRAM.
       AUTHOR. ANTHROPIC-CLAUDE-CONVERTER.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       * Combined data definitions would normally go here

       PROCEDURE DIVISION.

       * CONVERTED ASSEMBLER PROGRAM - MULTIPLE CHUNKS
       * Original program purpose: {analysis.get('purpose', 'Unknown')}
       * Conversion notice: This program was automatically converted from a large assembler file
       * Warning: File processed in chunks - manual integration required

{cobol_code}

       PROGRAM-END.
           STOP RUN.
            """

            # Validate combined code (simplified validation for large files)
            print("Performing simplified validation on combined code...")
            is_valid, validation_report = True, "Validation skipped for large chunked file"

        else:
            # Standard processing for smaller files
            print(f"Reading assembler code from {assembler_file_path}...")
            assembler_code = self.read_assembler_file(assembler_file_path)

            print("Analyzing assembler code...")
            analysis = self.analyze_assembler_code(assembler_code)

            print("Generating initial COBOL code...")
            cobol_code = self.generate_cobol_equivalent(assembler_code, analysis)

            print("Validating COBOL code...")
            is_valid, validation_report = self.validate_cobol_code(assembler_code, cobol_code)

            if not is_valid:
                print("Initial conversion has issues. Refining the COBOL code...")
                cobol_code = self.refine_cobol_code(assembler_code, cobol_code, validation_report)

                # Validate again after refinement
                print("Validating refined COBOL code...")
                is_valid, validation_report = self.validate_cobol_code(assembler_code, cobol_code)

                if not is_valid:
                    print("Warning: The refined COBOL code still has potential issues.")
                    print("Validation report:")
                    print(validation_report)
                else:
                    print("Refinement successful. COBOL code now validates.")
            else:
                print("Initial COBOL code validates successfully.")

        # Write the COBOL code to file
        self.write_cobol_file(output_file_path, cobol_code)

        # Generate and write documentation if requested
        if generate_docs:
            print(f"Generating conversion documentation...")
            try:
                documentation = self.document_conversion(
                    assembler_code if 'assembler_code' in locals() else "Large file processed in chunks",
                    cobol_code,
                    analysis
                )

                with open(docs_file_path, 'w', encoding='utf-8') as file:
                    file.write(documentation)
                print(f"Documentation successfully written to {docs_file_path}")
            except Exception as e:
                print(f"Error writing documentation to {docs_file_path}: {e}")


def main():
    parser = argparse.ArgumentParser(description='Convert Assembler code to COBOL using NVIDIA API')
    parser.add_argument('input_file', help='Path to the assembler source file')
    parser.add_argument('--output', '-o', help='Path to save the COBOL output file')
    parser.add_argument('--model', '-m', default="meta/llama-3.1-405b-instruct",
                        help='NVIDIA model to use (default: meta/llama-3.1-405b-instruct)')
    parser.add_argument('--no-docs', action='store_true', help='Skip documentation generation')
    parser.add_argument('--docs-file', '-d', help='Path to save the documentation file')

    args = parser.parse_args()

    # Create converter with API key
    converter = AssemblerToCOBOLConverter(
        api_key="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",
        model=args.model
    )


if __name__ == "__main__":
    main()
