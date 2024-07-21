<?php
/**
 * PHP-Erlang 接口，需要先安装ped
 * @author 彭 qq:249828165
 */
/*
 * 使用：
   $erl = new Erlang();
   $erl -> connect('xge1@127.0.0.1','xge');
   $erl -> get('lib_mail', 'send', '[~s,~u]', array(array($msg, (int)$data['role_id'])));
   $erl -> close();
ropeb_encode:
format_string

    The format string is composed of one or more directives.

    	    ~a - an atom
    	    ~s - a string  字符串直接使用这个或 ~b都行
    	    ~b - a binary (contain 0x0 in string)
    	    ~i - an integer  使用数字类型记得要(int)$val, intval($val)
    	    ~l - a long integer
    	    ~u - an unsigned long integer
    	    ~f - a float
    	    ~d - a double float
    	    ~p - an erlang pid

data

    The data to send to Erlang node. Initial wrapped with an array, tuple and list data must be wrapped with extra dimension.

*/
class Erlang
{
	protected $link;
	public $error = array();
	public $conn_error = array();
	public $cfg = array();
	private $node = 0;
	
	public function __construct($isExit = true)
	{
		$cfgCls = Config::getInstance();
		$this->cfg = $cfgCls->get('erl');
		//避免重复读取，有时会读取失败，直接提示退出
		if(!$_SESSION['admin_erlang_conn'])
		{
			$cfgCls->set('erl', $this->cfg);
			$_SESSION['admin_erlang_conn'] = $this->cfg;
		}else{
			$this->cfg = $_SESSION['admin_erlang_conn'];
		}
	}

	public function connect()
	{
		$cfg = $this->cfg;
		$node_name = $cfg['node'];
		$cookie = $cfg['cookie'];
		$this->link = ropeb_connect($node_name, $cookie, 5000);
		if(!$this->link)
		{
			$this->conn_error[$node_name] = 'could not connect['.ropeb_errorno().']:'  .  ropeb_error();
			$GLOBALS['erl_error'] .= "&nbsp;[$node_name]".$this->conn_error[$node_name];
		}
		return $this->link;
	}
	
	public function getError()
	{
		if($this->error)
		{
			$err = '';
			foreach($this->error as $node => $msg)
				$err .= "[{$node}]：{$msg}";
			
			
			return $err;
		}
		if(ropeb_error() == 'ei_connect error')
		{
			return '';
		}
		return ropeb_error();
	}
	
	public function getConnError()
	{
		if($this->conn_error)
		{
			$err = '';
			foreach($this->conn_error as $node => $msg)
				$err .= "[{$node}]：{$msg}";
				
			return $err;
		}
		return '';
	}
	
	private function rpc($M, $F, $A)
	{
		if(!is_resource($this->link))
		{
			$this->error[$this->node] = 'connect error!';
			return false;
		}
		try{
			$rpcRt = ropeb_rpc($M, $F, $A, $this->link);
			if(!$rpcRt)
				$rt = false;
			else
				$rt = ropeb_decode($rpcRt);
		}catch(Exception $e){
			$this->error[$this->node] .= $e -> getMessage();
			$rt = false;
		}
		return $rt;
	}

	public function newTable($name, $options)
	{
		$options = array_merge($options, array('named_table', 'public'));
		$x = ropeb_encode("[~a, [" . $this->repeatFormat('~a', $options) . "]]", array(array($name, $options)));
		return $this->rpc("ets", "new", $x);
	}

	/**
	 * 和erlang ets:insert使用一样
	 */
	public function insert($name, $key, $value)
	{
		$x = ropeb_encode("[~a, {~a, ~s}]", array(array($name, array($key, $value))));
		return $this->rpc("ets", "insert", $x);
	}

	public function bulkInsert($name, $values)
	{
		$x = ropeb_encode("[~a, [" . $this->repeatFormat('{~a, ~s}', count($values)) . "]]", array(array($name, $values)));
		return $this->rpc("ets", "insert", $x);
	}

	public function info($name, $item)
	{
		$x = ropeb_encode("[~a, ~a]", array(array($name, $item)));
		$rt = $this->rpc("ets", "info", $x);
		return isset($rt[1]) ? $rt : $rt[0];
	}

	public function delete_all_objects($name)
	{
		$x = ropeb_encode("[~a]", array(array($name)));
		return $this->rpc("ets", "delete_all_objects", $x);
	}

	/**
	 * 不建议使用，容易报错
	 * @param $name 表名
	 */
	public function tab2list($name)
	{
		$x = ropeb_encode("[~a]", array(array($name)));
		$rt = $this->rpc("ets", "tab2list", $x);
		return isset($rt[1]) ? $rt : $rt[0];
	}
	
	/**
	 * 调用某个模块的返回值,erlang端注意返回数字时要转换成list才能解包
	 * @param $model 如模块名：ets
	 * @param $func  如函数名：select
	 * @param $argk  ropeb_encode参数
	 * @param $argv  ropeb_encode参数值
	 */
	public function get($model, $func, $argk='[]', $argv=array())
	{
		$x = ropeb_encode($argk, $argv);
		$rt = $this->rpc($model, $func, $x);
		return isset($rt[1]) ? $rt : $rt[0];
	}	
	
	public function repeatFormat($format, $len)
	{
		if($len < 1)return '';
		return implode(', ', array_fill(0, $len, $format));
	}
	
	public function close()
	{
		return ropeb_close($this->link);
	}
	
	public function __destruct()
	{
		//return $this->close();
	}
}

