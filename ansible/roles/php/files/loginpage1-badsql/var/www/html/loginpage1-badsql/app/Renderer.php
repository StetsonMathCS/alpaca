<?php
namespace App;

class Renderer
{

    private $myTemplateDir = 'views/';
    private $myData = array();

    public function __construct($templateDir = null)
    {
        if ($templateDir !== null) {
            $this->myTemplateDir = $templateDir;
        }
    }

    /**
     * Render the given file in the stored directory
     * 
     * @param string $templateFile
     * @throws Exception
     */
    public function render($templateFile)
    {
        if (file_exists($this->myTemplateDir . $templateFile)) {
            include $this->myTemplateDir . $templateFile;
        } else {
            throw new Exception('no template file ' . $templateFile . ' present in directory ' . $this->myTemplateDir);
        }
    }

    /**
     * Magic setter to set any values that need to be rendered in the view
     * 
     * @param string $name
     * @param mixed $value
     */
    public function __set($name, $value)
    {
        $this->myData[$name] = $value;
    }

    /**
     * Magic getter to get value to render in the view
     * 
     * @param string $name
     * @return mixed
     */
    public function __get($name)
    {
        return $this->myData[$name];
    }
}
